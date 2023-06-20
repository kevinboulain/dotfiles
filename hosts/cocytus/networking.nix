{ config, ... }:
let
  network = {
    enable = true;
    networks."20-wired" = {
      matchConfig.Name = "enp3s0";
      DHCP = "ipv4";
      dhcpV4Config = {
        # Otherwise we would get a DNS from DHCP advertisements and
        # systemd-resolved would use that alongside the local Knot Resolver (see
        # resolvectl).
        UseDNS = assert config.services.krsed.enable; false;
      };
      # mDNS needs to be enabled at the link level when using systemd-networkd.
      networkConfig.MulticastDNS = true;
    };
  };
in
{
  systemd.network = network;

  # Enable SSH in the initrd.
  boot.initrd = {
    availableKernelModules = [ "r8169" ];
    network = {
      enable = true;
      # Evaluation will fail without:
      #  - boot.initrd.network.ssh.authorizedKeys (defaults to
      #    users.users.root.openssh.authorizedKeys.keys),
      #  - boot.initrd.network.ssh.hostKeys.
      # Once SSH'd in:
      #  systemd-tty-ask-password-agent
      ssh.enable = true;
    };

    # Stage 1 shares a common configuration with stage 2 (not done via
    # config.systemd.network directly to avoid duplicating list elements).
    systemd.network = network;
  };

  services.fail2ban = {
    enable = true;
    bantime-increment.enable = true;
  };
}