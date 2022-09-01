{ pkgs, ... }:
{
  environment.systemPackages = with pkgs; [
    iw
    mtr
    tcpdump
  ];

  networking.firewall.logRefusedPackets = true;  # Wasted my time debugging dropped packets...

  # DNS resolution is handed off to systemd.
  services.resolved.enable = true;
  # And enable mDNS (discovery of printers, chromecasts, ...).
  services.avahi = {
    enable = true;
    nssmdns = true;
    publish = {
      enable = true;
      addresses = true;
    };
  };

  services.openssh = {
    enable = true;
    extraConfig = ''
      # Only allow any users with privileges to log in remotely.
      AllowGroups root wheel
    '';
  };

  # A bridge without any physical interface serving as a gateway.
  # TAP interfaces can be attached for NAT.
  networking.bridges.nat0.interfaces = [];
  networking.interfaces.nat0 = {
    ipv4.addresses = [
      {
        address = "192.168.42.1";
        prefixLength = 24;
      }
    ];
  };
  networking.nat = {
    enable = true;
    enableIPv6 = true;
    internalInterfaces = [ "nat0" ];
  };

  # Set up dnsmasq to provide DHCP and DNS to clients attached to nat0.
  networking.firewall.allowedUDPPorts = [ 53 67 ]; # TODO: restrict to nat0
  services.dnsmasq = {
    enable = true;
    resolveLocalQueries = false;
    extraConfig = ''
      interface = nat0
      bind-interfaces
      dhcp-range = 192.168.42.100,192.168.42.200,1d
      dhcp-authoritative
    '';
  };
}
