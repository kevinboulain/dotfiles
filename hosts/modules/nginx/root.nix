{ config, lib, ... }:
let
  inherit (import ./lib.nix { inherit lib; }) mergeVirtualHostFragments virtualHostFragments;
in
{
  # A placeholder vhost for a root domain.
  # For example, some services need to configure locations like /.well-known on
  # the root domain and not their dedicated subdomain.
  services.nginx.virtualHosts.root = mergeVirtualHostFragments [
    virtualHostFragments.disallowRobots
    virtualHostFragments.emptyCatchAll
    virtualHostFragments.explicitServerName
    {
      default = true;
      enableACME = true;
      forceSSL = true;
    }
  ];
}
