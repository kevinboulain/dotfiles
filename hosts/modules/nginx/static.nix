{ config, lib, myStateDirectory, pkgs, ... }:
let
  inherit (import ./lib.nix { inherit lib; }) mergeVirtualHostFragments virtualHostFragments;
  root = "${myStateDirectory}/www";
in
{
  # A dumb configuration to serve files.
  services.nginx.virtualHosts.static = mergeVirtualHostFragments [
    virtualHostFragments.disallowRobots
    virtualHostFragments.emptyCatchAll
    {
      locations = {
        "~ \\.git".extraConfig = ''
           # https://git-scm.com/docs/git-http-backend
           client_max_body_size 0;
           include ${config.services.nginx.package}/conf/fastcgi_params;
           fastcgi_pass unix:/var/run/fcgiwrap.sock;
           fastcgi_param SCRIPT_FILENAME ${pkgs.git}/bin/git-http-backend;
           fastcgi_param GIT_HTTP_EXPORT_ALL "";
           fastcgi_param GIT_PROJECT_ROOT ${root};
           fastcgi_param PATH_INFO $uri;
        '';
        "/".extraConfig = ''
           root "${root}";
           try_files $uri =404;  # Only root would result in error logs for 404s.
        '';
      };
    }
  ];
  systemd.tmpfiles.rules = [ "d ${root} 1777 root root - -" ];
}
