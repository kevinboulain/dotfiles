{ ... }:
{
  # Rely on iwd for everything, including IP addressing.
  networking = {
    useDHCP = false;
    wireless.iwd = {
      enable = true;
      settings = {
        General = {
          AddressRandomization = "network";  # https://iwd.wiki.kernel.org/addressrandomization
          EnableNetworkConfiguration = true;
        };
        Network.EnableIPv6 = true;
      };
    };
  };

  # Prometheus should collect Wi-Fi statistics.
  services.prometheus.exporters.node.enabledCollectors = [ "wifi" ];
}
