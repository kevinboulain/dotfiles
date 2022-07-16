{ pkgs, ... }:
{
  # https://nixos.wiki/wiki/Sway
  programs.sway = {
    enable = true;
    wrapperFeatures.gtk = true;
    extraPackages = with pkgs; [
      foot
      fzf
      gammastep
      mako
      swayidle
      swaylock
    ];
  };

  environment.systemPackages = with pkgs; [
    firefox-wayland
    vlc
  ];
}
