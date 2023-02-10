{ config, lib, pkgs, withVPN ? false, ... }:
{
  environment.systemPackages = with pkgs; [
    dnsutils
    iw
    mtr
    tcpdump
  ] ++ lib.lists.optionals withVPN [
    mullvad  # CLI client.
  ];

  # TODO: all users can access account details.
  services.mullvad-vpn.enable = withVPN;

  # The dhcpcd module is a bit too inflexible.
  networking.useDHCP = false;

  networking.firewall = {
    enable = true;
    logRefusedPackets = true;  # Wasted my time debugging dropped packets...
  };

  # DNS resolution is handed off to systemd-resolved...
  services.resolved = {
    enable = true;
    dnssec = "true";
    llmnr = "false";
    fallbackDns = config.networking.nameservers;  # Never fallback to the compiled-in list.
    # Do not disable the stub listener: it ensures clients using
    # /etc/resolv.conf (e.g.: dig) are redirected to resolved and that the DNS
    # servers are used as configured.
  };
  # ...but it uses the local DNS server.
  # Note it's still possible to add a link-local DNS and it will have higher
  # priority if it specifies Domains=~. (like Mullvad does, see resolvectl).
  networking.nameservers = [ "::1" ];  # Sets the DNS= entry in resolved.conf.
  services.unbound = {
    enable = true;
    # Used by unbound-control and the Prometheus exporter.
    localControlSocketPath = "/run/unbound/controlpipe";  # That's how it's named in tests.
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
  services.prometheus.exporters.unbound = {
    enable = true;
    controlInterface = config.services.unbound.localControlSocketPath;
    listenAddress = "[::1]";  # The exporter written in Rust doesn't accept localhost.
    port = 9120;
   };
  systemd.services.prometheus-unbound-exporter.serviceConfig.SupplementaryGroups = [
    "unbound"  # To access the socket.
  ];
  services.prometheus.scrapeConfigs = [{
    job_name = "unbound";
    static_configs = [ { targets = [ "[::1]:9120" ]; } ];
  }];

  # Enable mDNS (discovery of printers, chromecasts, ...).
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
      # Only allow users with privileges to log in remotely.
      AllowGroups root wheel
    '';
  };
}
