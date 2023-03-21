# qemu-kvm -display curses -netdev bridge,br=nat0,id=net0,helper="$(which qemu-bridge-helper)" -device virtio-net-pci,netdev=net0 -m 4096M -drive file=path/to.iso,media=cdrom
{ config, lib, pkgs, ... }:
with lib;
let
  inherit (config.networking) noIPv6Internet;
in
{
  options.networking = {
    noIPv6Internet = mkOption {
      type = types.bool;
      default = false;
    };
  };

  config = {
    environment.systemPackages = with pkgs; [
      qemu
    ] ++ lists.optionals noIPv6Internet [
      jool-cli
    ];

    # It's still required to run:
    #  jool instance add --netfilter --pool6 64:ff9b::/96
    boot.extraModulePackages = with config.boot.kernelPackages; lists.optionals noIPv6Internet [
      jool
    ];

    # A bridge without any physical interface serving as a gateway.
    # TAP interfaces can be attached for NAT.
    networking.bridges.nat0.interfaces = [];
    networking.interfaces.nat0 = {
      ipv6.addresses = [{
        address = "fd2a::1";
        prefixLength = 64;
      }];
    };
    networking.nat = {
      enable = true;
      enableIPv6 = true;
      internalInterfaces = [ "nat0" ];
    };

    # Allow bridging nat0 from QEMU.
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

    # Send router advertisements to clients.
    # systemd-networkd 248 has support for RDNSS (IPv6AcceptRA{,.UseDNS}).
    services.radvd = {
      enable = assert config.services.unbound.enable; true;
      config = ''
      interface nat0 {
        IgnoreIfMissing off;
        AdvSendAdvert on;
        prefix fd2a::/64 {};
        route ::/0 {};
        RDNSS fd2a::1 {};
      };
    '';
    };
    # And allow to access the local Unbound.
    networking.firewall.interfaces.nat0.allowedUDPPorts = [ 53 ];
    services.unbound.settings.server = {
      interface = [ "fd2a::1" ];
      access-control = [ "fd2a::/64 allow" ];
    } // attrsets.optionalAttrs noIPv6Internet {
      module-config = ''"dns64 validator iterator"'';
      dns64-prefix = "64:ff9b::/96";
      # Ignore all AAAA records, necessary when there's truly no IPv6
      # connectivity.
      dns64-synthall = true;
    };
  };
}
