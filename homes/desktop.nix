{ lib, pkgs, ... }:
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
    ./minimal.nix
    ./modules/firefox.nix
    ./modules/gtk
    ./modules/sway
  ];

  home.packages = with pkgs; [
    # General desktop utilities.
    imv
    mpv
    pavucontrol
    zathura

    # Screenshots & co.
    grim
    slurp
    wl-clipboard

    # Mostly for Emacs.
    (mkWOFF2From { name = "iosevka-bin"; pkg = iosevka-bin; ext = "ttc"; })
  ];

  # Handle media key on bluetooth headsets.
  # https://wiki.archlinux.org/title/MPRIS#Bluetooth
  services.mpris-proxy.enable = true;

  # Until PipeWire implements D-Bus, some helpers used elsewhere.
  programs.bash.initExtra = lib.mkAfter ''
    volume_mute() {
      wpctl set-mute @DEFAULT_AUDIO_SINK@ toggle
    }
    volume_set() {
      local -ri current=''$(wpctl get-volume @DEFAULT_AUDIO_SINK@ | sed 's/Volume: \([[:digit:]]\)\.\([[:digit:]]\+\).*/\1\2/;s/^0\+//')
      local -ri new=''$((current+''${1?}))
      local -ri clamped=''$((new > 100 ? 100 : new < 0 ? 0 : new))
      wpctl set-volume @DEFAULT_AUDIO_SINK@ ''$clamped%
    }
    volume_up() { volume_set "''${1:-1}"; }
    volume_down() { volume_set -"''${1:-1}"; }
    declare -fx volume_mute volume_set volume_down volume_up
  '';
}
