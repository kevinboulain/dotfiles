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
      for file in ${pkg}/share/fonts/truetype/*.${ext}; do
        # FontForge and Nix will kill machines without too much ram so don't
        # parallelize this loop, at the detriment of beefier machines.
        fontforge --lang=ff -c 'Open($1); Generate($2);' "$file" "$woff2_directory"/"$(basename $file .${ext})".woff2
      done
    '';
  };
in
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
    (mkWOFF2From { name = "iosevka-bin"; pkg = iosevka-bin; ext = "ttc"; })
  ];
  fonts.fontconfig.enable = true;
}
