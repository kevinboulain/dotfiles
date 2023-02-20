{ pkgs, ... }:
{
  # Get rid of any package installed by default (like nano).
  environment.defaultPackages = [];

  # https://discourse.nixos.org/t/missing-man-pages/4680/6
  # Some, but not all, of these are true by default.
  documentation = {
    enable = true;
    dev.enable = true;
    man.enable = true;
  };

  environment.systemPackages = with pkgs; [
    man-pages
    man-pages-posix

    file
    lsof
    strace
    unzip

    emacs-nox
    gitFull  # I could (gitMinimal.override { sendEmailSupport = true; }) to do without the UI but that wouldn't be cached.
    hunspell
    hunspellDicts.en-us
    hunspellDicts.fr-any
    icdiff
    ripgrep
    stow
    tmux
  ];
  fonts.fonts = with pkgs; [
    # iosevka-bin ensures it's never rebuild from source but downloaded from
    # GitHub when not cached. Also, it embeds Iosevka Term (used in my
    # configurations).
    iosevka-bin
    (iosevka-bin.override { variant = "aile"; })
    (iosevka-bin.override { variant = "etoile"; })
  ];
}
