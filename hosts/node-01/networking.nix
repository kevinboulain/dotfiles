{ config, mySystemDirectory, ... }:
let
  network = {
    enable = true;
    networks."20-wired" = {
      matchConfig.Name = "enp0s6";
      DHCP = "ipv4";
      dhcpV4Config = {
        # Otherwise we would get a DNS from DHCP advertisements and
        # systemd-resolved would use that alongside the local Knot Resolver (see
        # resolvectl).
        UseDNS = assert config.services.kresd.enable; false;
      };
    };
  };
in
{
  systemd.network = network;

  # Enable SSH in the initrd.
  boot.initrd = {
    availableKernelModules = [ "virtio_pci" "virtio_net" ];
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
}
