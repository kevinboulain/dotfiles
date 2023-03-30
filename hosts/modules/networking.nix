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

  # DNS resolution is handed off to systemd-resolved (including mDNS)...
  services.resolved = {
    enable = true;
    dnssec = "true";
    llmnr = "false";
    fallbackDns = config.networking.nameservers;  # Never fallback to the compiled-in list.
    # Do not disable the stub listener: it ensures clients using
    # /etc/resolv.conf (e.g.: dig) are redirected to resolved and that the DNS
    # servers are used as configured.
  };
  networking.firewall.allowedUDPPorts = [ 5353 ];  # Allow mDNS.
  # ...but it uses the local DNS server.
  # Note it's still possible to add a link-local DNS and it will have higher
  # priority if it specifies Domains=~. (like Mullvad does, see resolvectl).
  networking.nameservers = [ "::1" ];  # Sets the DNS= entry in resolved.conf.
  services.unbound = {
    enable = true;
    # Used by unbound-control and the Prometheus exporter.
    # localControlSocketPath = "/run/unbound/controlpipe";  # That's how it's named in tests.
    settings = {
      server = {
        interface = [ "127.0.0.1" "::1" ];
        access-control = [ "127.0.0.1/32 allow" "::1/128 allow" ];
        extended-statistics = true;
      };
      forward-zone = {
        # Locally recursing isn't more privacy friendly than forwarding
        # everything to a (hopefully, trusted) DNS, as long as it's encrypted:
        # recursion is unlikely to ever be fully shielded.
        name = ".";
        forward-tls-upstream = true;
        forward-addr = [
          # https://mullvad.net/en/help/dns-over-https-and-dns-over-tls/
          "2a07:e340::2@853#doh.mullvad.net"
          "194.242.2.2@853#doh.mullvad.net"
        ];
      };
    };
  };
  # https://github.com/svartalf/unbound-telemetry has been archived and spams the logs.
  # services.prometheus.exporters.unbound = {
  #   enable = true;
  #   controlInterface = config.services.unbound.localControlSocketPath;
  #   listenAddress = "[::1]";  # The exporter written in Rust doesn't accept localhost.
  #   port = 9120;
  #  };
  # systemd.services.prometheus-unbound-exporter.serviceConfig.SupplementaryGroups = [
  #   "unbound"  # To access the socket.
  # ];
  # services.prometheus.scrapeConfigs = [{
  #   job_name = "unbound";
  #   static_configs = [ { targets = [ "[::1]:9120" ]; } ];
  # }];

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
