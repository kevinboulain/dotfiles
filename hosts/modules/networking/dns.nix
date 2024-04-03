{ config, lib, pkgs, ... }:
with lib;
let
  inherit (config.my) resolvers;
in
{
  options.my = {
    resolvers = mkOption {
      type = types.listOf (types.submodule {
        options = {
          ip = mkOption {
            type = types.nonEmptyStr;
          };
          hostname = mkOption {
            type = types.nonEmptyStr;
          };
        };
      });
      default = [
        # https://mullvad.net/en/help/dns-over-https-and-dns-over-tls/
        { ip = "194.242.2.2"; hostname = "doh.mullvad.net"; }
        { ip = "2a07:e340::2"; hostname = "doh.mullvad.net"; }
      ];
    };
  };

  config = {
    environment.systemPackages = with pkgs; [
      dnsutils
    ];

    # DNS resolution is handed off to systemd-resolved...
    services.resolved = {
      enable = true;
      # DNSSEC support in systemd is quite... special. For some context, see:
      # https://github.com/systemd/systemd/issues/25676
      # Enabling this option will result in often broken DNS resolutions.
      # dnssec = "true";
      llmnr = "false";
      fallbackDns = [ "" ];  # Never fallback to the compiled-in list.
      extraConfig = assert config.services.avahi.enable; ''
        # Conflicts with Avahi.
        MulticastDNS=false
      '';
      # Do not disable the stub listener: it ensures clients using
      # /etc/resolv.conf (e.g.: dig) are redirected to resolved and that the DNS
      # servers are used as configured.
    };
    # ...but it uses the local DNS server.
    # Note it's still possible to add a link-local DNS and it will have higher
    # priority if it specifies Domains=~. (like Mullvad does, see resolvectl).
    networking.nameservers = [ "::1" ];  # Sets the DNS= entry in resolved.conf.
    # Unbound doesn't allow to limit DNS64 to a view/interface/etc. dnsmasq
    # doesn't seem to support DNS64. The stable BIND version doesn't support TLS
    # forwarders. PowerDNS doesn't support AAAA exclusion without Lua scripting.
    services.kresd = {
      enable = true;
      package = pkgs.knot-resolver.override {
        # For cqueues (dependency of the HTTP module).
        extraFeatures = true;
      };
      listenPlain = [ "127.0.0.1:53" "[::1]:53" ];
      extraConfig = ''
        log_level('warning') -- info includes Prometheus scrapes.

        modules = {
         'dns64',
         'http', -- Prometheus.
         'policy',
         'stats',
         'view',
        }

        -- HTTP, including Prometheus.
        net.listen('127.0.0.1', 8453, { kind = 'webmgmt' })
        net.listen('::1', 8453, { kind = 'webmgmt' })

        -- Forward all requests via DoT.
        -- Locally recursing isn't more privacy friendly than forwarding
        -- everything to a (hopefully, trusted) DNS, as long as it's encrypted:
        -- recursion is unlikely to ever be fully shielded.
        policy.add(policy.all(policy.TLS_FORWARD({
          ${concatMapStringsSep ", " (resolver: "{ '${resolver.ip}', hostname='${resolver.hostname}' }" ) resolvers }
        })))

        -- Disable DNS64 by default.
        view:addr('0.0.0.0/0', policy.all(policy.FLAGS('DNS64_DISABLE')))
        view:addr('::/0', policy.all(policy.FLAGS('DNS64_DISABLE')))
      '';
    };

    # Enable mDNS (discovery of printers, chromecasts, ...).
    # resolved support is spotty: https://github.com/apple/cups/issues/5452
    services.avahi = {
      enable = true;
      nssmdns4 = true;
      nssmdns6 = true;
      publish = {
        enable = true;
        addresses = true;
      };
    };

    services.prometheus.scrapeConfigs = [{
      job_name = "kresd";
      static_configs = [ { targets = [ "localhost:8453" ]; } ];
    }];
  };
}
