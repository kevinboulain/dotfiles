{ pkgs, ... }:
{
  nixpkgs.overlays = [
    (import ../common/overlays/iosevka)
  ];

  environment.systemPackages = with pkgs; [
    file
    lsof

    emacs-nox
    gitFull  # I could (gitMinimal.override { sendEmailSupport = true; }) to do without the UI but that wouldn't be cached.
    hunspell
    hunspellDicts.en-us
    hunspellDicts.fr-any
    stow
    tmux
  ];
  fonts.fonts = with pkgs; [
    iosevka-term  # Used in my configurations.
  ];
}
