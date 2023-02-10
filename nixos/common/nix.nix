{ config, pkgs, ... }:
{
  environment.variables = {
    NIX_SHELL_PRESERVE_PROMPT = "1";
  };

  environment.systemPackages = with pkgs; [
    cntr  # https://nixos.org/manual/nixpkgs/stable/#breakpointhook
    nix-diff  # To compare derivations, e.g.: nix-diff /nix/var/nix/profiles/system-{1,2}-link
    nix-index  # To search packages.
    nix-tree  # Convenient dependency graphs.
    nixos-option  # To evaluate the configuration.
    nvd  # To compare system images, e.g.: nvd diff /nix/var/nix/profiles/system-{1,2}-link
  ];

  # Prevent anyone not in the wheel group from connecting to the Nix daemon.
  # See also trusted-users, which allows root by default.
  nix.settings.allowed-users = [ "@wheel" ];

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
