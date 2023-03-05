{ pkgs, ... }:
let
  # https://github.com/NixOS/nixpkgs/issues/182465#issuecomment-1207234828
  mkWOFF2From = { name, pkg, ext }: pkgs.stdenvNoCC.mkDerivation {
    name = "${name}-woff2";
    nativeBuildInputs = [ pkgs.fontforge pkg ];
    dontInstall = true;
    unpackPhase = ''
      woff2_directory=$out/share/fonts/woff2/
      mkdir -p "$woff2_directory"
      pids=()
      for file in ${pkg}/share/fonts/truetype/*.${ext}; do
        fontforge --lang=ff -c 'Open($1); Generate($2);' "$file" "$woff2_directory"/"$(basename $file .${ext})".woff2 &
        pids+=($!)
      done
      for pid in "''${pids[@]}"; do
        wait "$pid"
      done
    '';
  };
in
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
    (mkWOFF2From { name = "iosevka-bin"; pkg = iosevka-bin; ext = "ttc"; })
  ];
}
