{ config, lib, myLib, mySystemDirectory, pkgs, ... }:
let
  inherit (myLib) state;
in
{
  imports = [ ./dns.nix ];

  environment.systemPackages = with pkgs; [
    iw
    mtr
    tcpdump
  ];

  systemd.services.mullvad-daemon.environment = {
    # https://github.com/mullvad/mullvadvpn-app/#environment-variables-used-by-the-service
    MULLVAD_MANAGEMENT_SOCKET_GROUP = "wheel";
  };
  # Stores the account number.
  fileSystems = state.binds (lib.optional config.services.mullvad-vpn.enable "/etc/mullvad-vpn");

  # The dhcpcd module is a bit too inflexible.
  networking.useDHCP = false;

  networking.firewall = {
    enable = true;
    logRefusedPackets = true;  # Wasted my time debugging dropped packets...
  };

  services.openssh = {
    enable = true;
    extraConfig = ''
      # Don't allow anyone by default.
      AllowUsers !*
    '';
    hostKeys = [{
      # Not a bind mount because /etc/ssh hosts other things we don't care
      # about, like symlinks to /etc/static. The directory is created for us.
      path = "${mySystemDirectory}/etc/ssh/ssh_host_ed25519_key";
      type = "ed25519";
    }];
  };
}
