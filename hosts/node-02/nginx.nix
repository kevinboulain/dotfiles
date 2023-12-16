{ ... }: {
  services.nginx.virtualHosts.static = {
    serverName = "static.boula.in";
    enableACME = true;
    forceSSL = true;
  };
}
