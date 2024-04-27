# qemu-kvm -display curses -netdev bridge,br=nat0,id=net0,helper="$(which qemu-bridge-helper)" -device virtio-net-pci,netdev=net0 -m 4096M -drive file=path/to.iso,media=cdrom
{
  config,
  lib,
  pkgs,
  ...
}:
with lib;
let
  inherit (config.my) noIPv6Internet;
in
{
  options.my = {
    noIPv6Internet = mkOption {
      type = types.bool;
      default = false;
    };
  };

  config = {
    environment.systemPackages = with pkgs; [ qemu ] ++ lists.optionals noIPv6Internet [ jool-cli ];

    # It's still required to run:
    #  jool instance add --netfilter --pool6 64:ff9b::/96
    boot.extraModulePackages = with config.boot.kernelPackages; lists.optionals noIPv6Internet [ jool ];

    # A bridge without any physical interface serving as a gateway.
    # TAP interfaces can be attached for NAT.
    networking.bridges.nat0.interfaces = [ ];
    networking.interfaces.nat0 = {
      ipv6.addresses = [
        {
          address = "fd2a::1";
          prefixLength = 64;
        }
      ];
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
      enable =
        assert config.services.kresd.enable;
        true;
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
    # And allow to access the local Knot Resolver.
    networking.firewall.interfaces.nat0.allowedUDPPorts = [ 53 ];
    services.kresd = {
      listenPlain = [ "[fd2a::1]:53" ];
      extraConfig = mkAfter ''
        dns64.config({
          prefix = '64:ff9b::', -- /96
          -- Ignore AAAA records (for example, when there's truly no IPv6
          -- connectivity and IPv4 has to be forced).
          exclude_subnets = { '::ffff/96', ${strings.optionalString noIPv6Internet "'::/0',"} },
        })
        -- Allow DNS64 for this network.
        view:addr('fd2a::/64', policy.all(policy.FLAGS(nil, 'DNS64_DISABLE')))
      '';
    };
  };
}
