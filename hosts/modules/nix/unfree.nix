{ config, lib, ... }:
with lib;
let
  inherit (config.my) allowedUnfreePackages;
in
{
  options.my = {
    # Instead of broadly allowing all unfree packages via
    # nixpkgs.config.allowUnfree, ensure I explicitly allow the ones I want.
    allowedUnfreePackages = mkOption {
      type = types.listOf types.str;
      default = [];
    };
  };

  config = mkIf (allowedUnfreePackages != [])  {
    nixpkgs.config.allowUnfreePredicate = pkg: builtins.elem (getName pkg) allowedUnfreePackages;
  };
}
