{ lib, pkgs, ... }: {
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

    # More fonts.
    (iosevka-bin.override { variant = "aile"; })
    (iosevka-bin.override { variant = "etoile"; })
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
