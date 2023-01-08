{ pkgs, ... }:
{
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
    # iosevka-bin ensures it's never rebuild from source but downloaded from
    # GitHub when not cached. Also, it embeds Iosevka Term (used in my
    # configurations).
    iosevka-bin
    (iosevka-bin.override { variant = "aile"; })
    (iosevka-bin.override { variant = "etoile"; })
  ];
}
