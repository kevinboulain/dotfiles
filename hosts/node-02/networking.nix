{ config, mySystemDirectory, ... }:
let
  network = {
    enable = true;
    networks."20-wired" = {
      matchConfig.Name = "enp1s0";
      DHCP = "ipv4";
      dhcpV4Config = {
        # Otherwise we would get a DNS from DHCP advertisements and
        # systemd-resolved would use that alongside the local Knot Resolver (see
        # resolvectl).
        UseDNS =
          assert config.services.kresd.enable;
          false;
      };
      # No RA nor DHCPv6...
      address = [ "2a01:4f9:c012:57dd::1/64" ];
      gateway = [ "fe80::1" ];
    };
  };
in
{
  systemd.network = network;

  # Enable SSH in the initrd.
  boot.initrd = {
    availableKernelModules = [
      "virtio_pci"
      "virtio_net"
    ];
    # Stage 1 shares a common configuration with stage 2 (not done via
    # config.systemd.network directly to avoid duplicating list elements).
    systemd.network = network;
    # Evaluation will fail without:
    #  - boot.initrd.network.ssh.authorizedKeys (defaults to
    #    users.users.root.openssh.authorizedKeys.keys),
    #  - boot.initrd.network.ssh.hostKeys.
    # Once SSH'd in:
    #  systemd-tty-ask-password-agent
    network.ssh = {
      enable = true;
      hostKeys = [ "${mySystemDirectory}/etc/ssh/ssh_initrd_ed25519_key" ];
    };
  };

  services.fail2ban = {
    enable = true;
    bantime-increment.enable = true;
  };

  my.resolvers = [
    # Ideally, the Mullvad DNS wouldn't block me for sending too many requests
    # when I'm not on their VPN but Matrix is quite spammy in large rooms.
    # https://developers.cloudflare.com/1.1.1.1/encryption/dns-over-tls/
    {
      ip = "1.1.1.1";
      hostname = "one.one.one.one";
    }
    {
      ip = "1.0.0.1";
      hostname = "one.one.one.one";
    }
    {
      ip = "2606:4700:4700::1111";
      hostname = "one.one.one.one";
    }
    {
      ip = "2606:4700:4700::1001";
      hostname = "one.one.one.one";
    }
  ];
}
