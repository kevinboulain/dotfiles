{ config, ... }:
{
  systemd.network = {
    enable = true;
    networks."20-wired" = {
      matchConfig.Name = "enp3s0";
      DHCP = "ipv4";
      dhcpV4Config = {
        # Otherwise we would get a DNS from DHCP advertisements and
        # systemd-resolved would use that alongside the local Unbound (see
        # resolvectl).
        UseDNS = assert config.services.unbound.enable; false;
      };
    };
  };
}
