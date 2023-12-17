{ config, ... }:
let
  inherit (import ./lib.nix { inherit config; }) configuration;
in
{
  # https://matrix-org.github.io/synapse/latest/delegate.html
  services.nginx.virtualHosts.root.locations."~ ^/.well-known/matrix/(client|server)$".extraConfig = ''
    add_header Access-Control-Allow-Origin *;
    default_type application/json;
    return 200 '${builtins.toJSON configuration}';
  '';
}
