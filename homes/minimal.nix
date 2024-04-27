{ pkgs, ... }:
{
  imports = [
    ./modules/bash
    ./modules/emacs
    ./modules/gdb
    ./modules/git.nix
    ./modules/tmux
  ];

  home.packages = with pkgs; [
    icdiff
    ripgrep
    # iosevka-bin ensures it's never rebuild from source but downloaded from
    # GitHub when not cached. Also, it embeds Iosevka Term (for example, used by
    # htmlize in Emacs).
    iosevka-bin
  ];
  fonts.fontconfig.enable = true;
}
