{ lib, ... }:
let
  # 499 is a special case of 502 and is refused by nginx.
  errorCodes = map builtins.toString ((lib.range 400 498) ++ (lib.range 500 599));
  emptyLocation = code: {
    # 'add_header' would add another content-type header instead of overwriting
    # the default.
    extraConfig = ''
      types {} default_type 'text/html';
      return ${builtins.toString code} '<!DOCTYPE html><html><head><title>...</title></head><body style="background-color:black;"></body></html>';
    '';
  };
in
{
  # A dumb configuration to serve files.
  services.nginx = {
    enable = true;
    virtualHosts.localhost = {
      extraConfig = builtins.concatStringsSep "\n" (map (code: ''error_page ${code} @${code};'') errorCodes);
      locations = {
        "= /" = emptyLocation 200;
        "= /robots.txt".extraConfig = ''
          add_header X-Robots-Tag "noindex";
          return 200 'User-agent: *\nDisallow: /\n';
        '';
        "/".root = "/srv/www";
      } // builtins.listToAttrs (map
        (code: { name = "@" + code; value = emptyLocation code; })
        errorCodes);
    };
  };
  networking.firewall.allowedTCPPorts = [ 80 ];
  systemd.tmpfiles.rules = [
    "d /srv/www 1777 root root - -"
  ];
}
