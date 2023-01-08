{ lib, ... }:
let
  # 499 is a special case of 502 and is refused by nginx.
  errorCodes = map builtins.toString ((lib.range 400 498) ++ (lib.range 500 599));
  emptyLocation = code: {
    # 'add_header' would add another content-type header instead of overwriting
    # the default.
    extraConfig = ''
      types {} default_type 'text/html';
      return ${builtins.toString code} '<!DOCTYPE html><html><head><title>...</title></head><body style="background-color:black;"></body></html>';
    '';
  };
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
  # A dumb configuration to serve files.
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
    virtualHosts.localhost = {
      extraConfig = builtins.concatStringsSep "\n" (map (code: ''error_page ${code} @${code};'') errorCodes);
      locations = {
        "= /" = emptyLocation 200;
        "= /robots.txt".extraConfig = ''
          add_header X-Robots-Tag "noindex";
          return 200 'User-agent: *\nDisallow: /\n';
        '';
        "/".extraConfig = ''
           root "/srv/www";
           try_files $uri =404;  # Only root would result in error logs for 404s.
        '';
      } // builtins.listToAttrs (map
        (code: { name = "@" + code; value = emptyLocation code; })
        errorCodes);
    };
  };
  networking.firewall.allowedTCPPorts = [ 80 ];
  systemd.tmpfiles.rules = [
    "d /srv/www 1777 root root - -"
  ];

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
