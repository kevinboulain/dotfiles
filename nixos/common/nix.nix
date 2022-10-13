{ config, pkgs, ... }:
{
  environment.systemPackages = with pkgs; [
    cntr  # https://nixos.org/manual/nixpkgs/stable/#breakpointhook
    nix-index  # To search packages.
    nixos-option  # To evaluate the configuration.
  ];

  # Set up ccache. Note package need to opt-in with programs.ccache.packageNames
  # and might have to overwrite the derivation's stdenv with ccacheStdenv.
  # Get statistics with:
  #  CCACHE_DIR=/var/cache/ccache ccache --show-stats
  programs.ccache.enable = true;
  nix.settings.extra-sandbox-paths = [ config.programs.ccache.cacheDir ];
  systemd.tmpfiles.rules = [
    # This directory isn't automatically created:
    # https://github.com/NixOS/nixpkgs/blob/master/nixos/modules/programs/ccache.nix#L58
    "d ${config.programs.ccache.cacheDir} 770 root nixbld - -"
  ];
}
