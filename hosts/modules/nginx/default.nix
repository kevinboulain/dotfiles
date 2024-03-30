{ config, lib, ... }:
let
  # Most of these are for the nginxlog Prometheus exporter and this is mostly an
  # extension of the default 'combined' log format:
  # https://docs.nginx.com/nginx/admin-guide/monitoring/logging/#setting-up-the-access-log
  # https://github.com/martin-helmich/prometheus-nginxlog-exporter#collected-metrics
  #  - added $server_name for the vhost ($host is sent by the client)
  #  - removed '-'
  #  - removed $time_local since it's added by syslog
  #  - added $request_length, $request_time, $upstream_connect_time,
  #    $upstream_header_time, $upstream_response_time for the exporter
  accessFormat =
    "$server_name $remote_addr $remote_user \"$request\" $request_length $status $bytes_sent $body_bytes_sent \"$http_referer\" \"$http_user_agent\" " +
      "rt=$request_time uct=$upstream_connect_time uht=$upstream_header_time urt=$upstream_response_time";
in
{
  services.nginx = {
    enable = true;
    commonHttpConfig = ''
      log_format access_format '${accessFormat}';
      # Exclusively log to syslog:
      #  - added nohostname since it's added by syslog when using UNIX sockets
      error_log syslog:server=unix:/dev/log,nohostname warn;
      access_log syslog:server=unix:/dev/log,nohostname access_format;
      # And also send a copy to the nginxlog Prometheus exporter:
      access_log syslog:server=[::1]:9110 access_format;
    '';
  };
  networking.firewall.allowedTCPPorts = [ 80 ] ++ (lib.optional config.security.acme.acceptTerms 443);

  security.acme.defaults.reloadServices = [ "nginx" ];

  services.prometheus.exporters.nginxlog = {
    enable = true;
    listenAddress = "localhost";
    port = 9110;
    settings.namespaces = [{
      format = accessFormat;
      source.syslog = {
        listen_address = "udp://[::1]:9110";  # Uses ResolveUDPAddr and can't listen on all addresses.
        format = "rfc3164";
        tags = [ "nginx" ];  # Necessary.
      };
      # nginx is configured to log all vhosts to syslog so merge everything
      # under a single metric.
      # https://github.com/martin-helmich/prometheus-nginxlog-exporter#advanced-features
      name = "nginx";
      metrics_override = { prefix = "nginx"; };
      relabel_configs = [
        { target_label = "server_name"; from = "server_name"; }
      ];
    }];
  };
  services.prometheus.scrapeConfigs = [{
    job_name = "nginx";
    static_configs = [ { targets = [ "localhost:9110" ]; } ];
  }];
}
