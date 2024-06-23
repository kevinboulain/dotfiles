{ lib, pkgs, ... }:
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
    pulseaudio # For pactl, see below.
    zathura

    # Screenshots & co.
    grim
    slurp
    wl-clipboard
  ];

  # Handle media key on bluetooth headsets.
  # https://wiki.archlinux.org/title/MPRIS#Bluetooth
  services.mpris-proxy.enable = true;

  # Until PipeWire implements D-Bus, some helpers used elsewhere.
  # Also, I'm using pactl instead of wpctl because PipeWire often breaks on
  # Steam and I revert to PulseAudio in this case...
  programs.bash.initExtra = lib.mkAfter ''
    volume_mute() {
      pactl set-sink-mute @DEFAULT_SINK@ toggle
    }
    volume_set() {
      local -ri current=''$(pactl get-sink-volume @DEFAULT_SINK@ | grep -Po '\d+(?=%)' | head -1)
      local -ri new=''$((current+''${1?}))
      local -ri clamped=''$((new > 100 ? 100 : new < 0 ? 0 : new))
      pactl set-sink-volume @DEFAULT_SINK@ ''$clamped%
    }
    volume_up() { volume_set "''${1:-1}"; }
    volume_down() { volume_set -"''${1:-1}"; }
    declare -fx volume_mute volume_set volume_down volume_up
  '';
}
