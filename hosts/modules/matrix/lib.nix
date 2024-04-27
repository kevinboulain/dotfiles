{ config, ... }:
let
  matrixDomain = config.services.nginx.virtualHosts.matrix.serverName;
  rootDomain = config.services.nginx.virtualHosts.root.serverName;
in
{
  configuration = {
    # It's fine to return both the client and the server configurations.
    "m.homeserver" = {
      base_url = "https://${matrixDomain}";
      server_name = rootDomain;
    };
    "m.server" = "${matrixDomain}:443";
  };
}
