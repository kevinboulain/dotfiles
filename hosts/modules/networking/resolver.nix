# While preferable, the local resolver doesn't have to always be enabled (and
# when cross-compiling, the LLVM build takes a while).
{ config, pkgs, ... }: {
  # Never fallback to the compiled-in list.
  services.resolved.fallbackDns = config.networking.nameservers;

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
      log_level('info')

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
        -- https://mullvad.net/en/help/dns-over-https-and-dns-over-tls/
        { '194.242.2.2', hostname='doh.mullvad.net' },
        { '2a07:e340::2', hostname='doh.mullvad.net' },
      })))

      -- Disable DNS64 by default.
      view:addr('0.0.0.0/0', policy.all(policy.FLAGS('DNS64_DISABLE')))
      view:addr('::/0', policy.all(policy.FLAGS('DNS64_DISABLE')))
    '';
  };

  services.prometheus.scrapeConfigs = [{
    job_name = "kresd";
    static_configs = [ { targets = [ "localhost:8453" ]; } ];
  }];
}
