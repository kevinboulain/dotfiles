{ ... }:
{
  # Rely on iwd for everything, including IP addressing.
  networking = {
    useDHCP = false;
    wireless.iwd = {
      enable = true;
      settings = {
        General.EnableNetworkConfiguration = true;
        Network.EnableIPv6 = true;
      };
    };
  };

  # Prometheus should collect Wi-Fi statistics.
  services.prometheus.exporters.node.enabledCollectors = [ "wifi" ];
}
