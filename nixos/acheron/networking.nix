arguments@{ config, ... }:
let
  inherit (import ../common/lib.nix arguments) mount;
in
{
  # Rely on iwd for everything, including IP addressing.
  networking.wireless.iwd = {
    enable = true;
    settings = {
      General = {
        AddressRandomization = "network";  # https://iwd.wiki.kernel.org/addressrandomization
        EnableNetworkConfiguration = true;
      };
      Network = {
        EnableIPv6 = true;
        # Otherwise we would get a DNS from DHCP advertisements and
        # systemd-resolved would use that alongside the local Unbound (see
        # resolvectl).
        NameResolvingService = assert config.services.unbound.enable; "none";
      };
    };
  };
  fileSystems = mount.systemBinds [
    # Stores SSIDs and PSKs.
    "/var/lib/iwd"
  ];

  # Prometheus should collect Wi-Fi statistics.
  services.prometheus.exporters.node.enabledCollectors = [ "wifi" ];
}
