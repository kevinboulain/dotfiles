{ lib, ... }: rec {
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

  mergeVirtualHostFragments = lib.lists.foldl (a: b: lib.attrsets.recursiveUpdate a b) {};

  virtualHostFragments = {
    disallowRobots.locations."= /robots.txt".extraConfig = ''
      add_header X-Robots-Tag "noindex";
      return 200 'User-agent: *\nDisallow: /\n';
    '';

    emptyCatchAll = {
      extraConfig = builtins.concatStringsSep "\n" (map (code: ''error_page ${code} @${code};'') errorCodes);
      locations = {
        "= /" = emptyLocation 200;
      } // builtins.listToAttrs (map
        (code: { name = "@" + code; value = emptyLocation code; })
        errorCodes);
    };

    # Sadly, this doesn't tell which vhost is incorrectly configured but I still
    # prefer that to copy/pasting this throw.
    explicitServerName.serverName = lib.mkDefault (throw "serverName must be set");
  };
}
