#!/usr/bin/env python3

from typing import Callable, Dict, Generator

import prometheus_api_client as prometheus


Block = Dict[str, str]
Blocks = Generator[None, None, Block]

PROMETHEUS = prometheus.PrometheusConnect(url='http://localhost:9090')


def prometheus_snapshot(query: str) -> prometheus.MetricSnapshotDataFrame:
    return prometheus.MetricSnapshotDataFrame(PROMETHEUS.get_current_metric_value(query))


def prometheus_query_range(query: str) -> prometheus.MetricRangeDataFrame:
    return prometheus.MetricRangeDataFrame(PROMETHEUS.custom_query(query))


def interpolate(minimum, maximum, value) -> float:
    value = max(minimum, min(maximum, value))
    return (value - minimum) / (maximum - minimum)


def unicode_bar(value, minimum, maximum, *, converter=int,
                bars=(' ', '\u2581', '\u2582', '\u2583', '\u2584', '\u2585', '\u2586', '\u2587', '\u2588')) -> str:
    return bars[converter(interpolate(minimum, maximum, value) * (len(bars)-1))]


def span(text, **kwargs: Dict[str, str]) -> str:
    # TODO: escaping
    attributes = ' '.join(f'{key}="{value}"' for key, value in kwargs.items())
    return f'<span {attributes}>{text}</span>'


def block(full_text: str, *, markup: str = 'pango') -> Block:
    return {
        'full_text': full_text,
        'markup': markup,
    }


def audio() -> Blocks:
    import re
    import subprocess

    try:
        output = subprocess.check_output(['pactl', 'get-sink-mute', '@DEFAULT_SINK@'])
        if b'yes' in output:
            volume = 0
        else:
            output = subprocess.check_output(['pactl', 'get-sink-volume', '@DEFAULT_SINK@'])
            matches = re.findall(rb'(\d+)%.*', output)
            if not matches:
                return
            volume = int(matches[0])
        bar = unicode_bar(volume, 0, 100)
        if volume > 100:
            bar = span(bar, color='red')
    except subprocess.CalledProcessError:
        bar = '†'
    yield block(f'{bar} Volume')


def batteries() -> Blocks:
    frame = prometheus_snapshot('node_power_supply_capacity')
    for power_supply, capacity in zip(frame.power_supply, frame.value):
        power_supply = power_supply.lower()
        capacity = int(float(capacity))
        bar = unicode_bar(capacity, 0, 100)
        if capacity <= 10:
            bar = span(bar, color='red')
        elif capacity <= 30:
            bar = span(bar, color='orange')
        yield block(f'{bar} {capacity}% {power_supply}')


def cpu() -> Blocks:
    frame = prometheus_query_range('avg (sum (rate(node_cpu_seconds_total{mode!="idle"}[1m])) without (mode)) without (cpu) [5m:]')
    bars = ''
    for second in frame.value:
        utilization = int(float(second)*100)
        bar = unicode_bar(utilization, 0, 100)
        if utilization > 70:
            bar = span(bar, color='red')
        elif utilization > 30:
            bar = span(bar, color='orange')
        bars += bar
    if bars:
        yield block(f'{bars} {utilization}% CPU')


def network() -> Blocks:
    for query, kind in [
            ('rate(node_network_transmit_bytes_total{device="wlan0"}[1m]) [5m:]', 'Upload'),
            ('rate(node_network_receive_bytes_total{device="wlan0"}[1m]) [5m:]', 'Download'),
    ]:
        frame = prometheus_query_range(query)
        bars = ''
        for bytes in frame.value:
            bytes = int(float(bytes))
            bars += unicode_bar(bytes, 0, 1*1000*1000)  # 1M is interesting but not too large.
        if bars:
            yield block(f'{bars} {bytes/1000/1000:.2f}M {kind}')


def temperature() -> Blocks:
    frame = prometheus_query_range('max (node_thermal_zone_temp) without (type, zone) [5m:]')
    bars = ''
    for temperature in frame.value:
        temperature = int(float(temperature))
        bar = unicode_bar(temperature, 30, 100)  # It's unlikely to be less than 30°C.
        if temperature > 70:
            bar = span(bar, color='red')
        elif temperature > 50:
            bar = span(bar, color='orange')
        bars += bar
    if bars:
        yield block(f'{bars} {temperature}°C')


def wifi() -> Blocks:
    # TODO: device specific
    #  https://github.com/bmegli/wifi-scan/issues/18
    #  https://www.intuitibits.com/2016/03/23/dbm-to-percent-conversion/
    frame = prometheus_snapshot('node_wifi_station_info{mode="client"}')
    ssid_by_bssid = {
        bssid: ssid
        for bssid, ssid, value in zip(frame.bssid, frame.ssid, frame.value)
        if int(value)
    }
    frame = prometheus_snapshot('node_wifi_station_signal_dbm')
    for mac_address, dbm in zip(frame.mac_address, frame.value):
        if mac_address not in ssid_by_bssid:
            continue  # Just in case.
        signal = int(interpolate(-110, -40, float(dbm)) * 100)
        ssid = ssid_by_bssid[mac_address]
        bar = unicode_bar(signal, 0, 100)
        if signal <= 10:
            bar = span(bar, color='red')
        elif signal <= 30:
            bar = span(bar, color='orange')
        yield block(f'{bar} {signal}% {ssid}')


def main():
    import json
    import time
    import traceback

    def cache_for(seconds: float, function: Callable[..., Blocks], *args, **kwargs) -> Blocks:
        blocks, start = [], None
        def wrapper():
            nonlocal blocks, start
            if start is None or time.time() - start >= seconds:
                start = time.time()
                blocks = list(function(*args, **kwargs))
            yield from blocks
        return wrapper

    audio_ = cache_for(5, audio)
    batteries_ = cache_for(30, batteries)
    cpu_ = cache_for(30, cpu)
    network_ = cache_for(30, network)
    temperature_ = cache_for(30, temperature)
    wifi_ = cache_for(15, wifi)

    print(json.dumps({'version': 1}))
    print('[')
    while True:
        try:
            print(json.dumps([
                *network_(),
                *temperature_(),
                *cpu_(),
                *wifi_(),
                *audio_(),
                *batteries_(),
                block(time.strftime('%T %A %F')),
            ]), end=',\n')
        except Exception:
            with open('/tmp/status_command.log', 'a') as handle:
                traceback.print_exc(file=handle)
        time.sleep(1)
    print(']')


if __name__ == '__main__':
    main()
