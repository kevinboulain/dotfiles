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
      # mDNS needs to be enabled at the link level when using systemd-networkd.
      networkConfig.MulticastDNS = true;
    };
  };

  services.fail2ban = {
    enable = true;
    bantime-increment.enable = true;
  };
}
