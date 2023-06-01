{ config, lib, myLib, mySystemDirectory, pkgs, ... }:
let
  inherit (myLib) state;
in
{
  environment.systemPackages = with pkgs; [
    dnsutils
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

  # DNS resolution is handed off to systemd-resolved (including mDNS).
  services.resolved = {
    enable = true;
    dnssec = "true";
    llmnr = "false";
    # Do not disable the stub listener: it ensures clients using
    # /etc/resolv.conf (e.g.: dig) are redirected to resolved and that the DNS
    # servers are used as configured.
  };
  networking.firewall.allowedUDPPorts = [ 5353 ];  # Allow mDNS.

  services.openssh = {
    enable = true;
    extraConfig = ''
      # Only allow users with privileges to log in remotely.
      AllowGroups root wheel
    '';
    hostKeys = [{
      # Not a bind mount because /etc/ssh hosts other things we don't care
      # about, like symlinks to /etc/static. The directory is created for us.
      path = "${mySystemDirectory}/etc/ssh/ssh_host_ed25519_key";
      type = "ed25519";
    }];
  };
}
