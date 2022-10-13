{ pkgs, ... }:
{
  nixpkgs.overlays = [
    (import ../common/overlays/iosevka)
  ];

  # Get rid of any package installed by default (like nano).
  environment.defaultPackages = [];

  environment.systemPackages = with pkgs; [
    file
    lsof
    strace

    emacs-nox
    gitFull  # I could (gitMinimal.override { sendEmailSupport = true; }) to do without the UI but that wouldn't be cached.
    hunspell
    hunspellDicts.en-us
    hunspellDicts.fr-any
    stow
    tmux
  ];
  fonts.fonts = with pkgs; [
    iosevka
    # Used in my configurations, provides correct spacing of some characters in
    # terminals. Sadly, it's a custom package so it's not cached (and takes time
    # to build).
    iosevka-term
  ];
}
