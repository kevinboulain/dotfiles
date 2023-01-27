# qemu-kvm -display curses -netdev bridge,br=nat0,id=net0,helper="$(which qemu-bridge-helper)" -device virtio-net-pci,netdev=net0 -m 1024M -drive file=debian-live.iso,media=cdrom
{ pkgs, ... }:
{
  environment.systemPackages = with pkgs; [
    qemu
  ];

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
  networking.firewall.interfaces.nat0.allowedUDPPorts = [ 53 67 ];
  services.dnsmasq = {
    enable = true;
    resolveLocalQueries = false;
    settings = {
      interface = "nat0";
      bind-interfaces = true;
      dhcp-range = "192.168.42.100,192.168.42.200,1d";
      dhcp-authoritative = true;
    };
  };

  environment.etc."qemu/bridge.conf" = {
    mode = "0644";
    text = ''
      allow nat0
    '';
  };
  security.wrappers.qemu-bridge-helper = {
    capabilities = "cap_net_admin+p";
    owner = "root";
    group = "root";
    source = "${pkgs.qemu}/libexec/qemu-bridge-helper";
  };
}
