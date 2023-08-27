{ myLib, ... }:
let
  inherit (myLib) state;
in
{
  fileSystems = state.binds [
    # Stores timeseries.
    "/var/lib/prometheus2"
  ];

  services.prometheus = {
    enable = true;
    listenAddress = "localhost";
    globalConfig = {
      scrape_interval = "30s";
      evaluation_interval = "30s";
    };
    retentionTime = "10y";
    scrapeConfigs = [
      {
        job_name = "prometheus";
        static_configs = [ { targets = [ "localhost:9090" ]; } ];
      }
      {
        job_name = "node_exporter";
        static_configs = [ { targets = [ "localhost:9100" ]; } ];
      }
    ];
  };
  services.prometheus.exporters.node = {
    enable = true;
    listenAddress = "localhost";
  };
}
