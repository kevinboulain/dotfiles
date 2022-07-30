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
    grim
    slurp
    vlc
  ];

  # For screensharing (via WebRTC).
  # https://discourse.nixos.org/t/some-loose-ends-for-sway-on-nixos-which-we-should-fix/17728
  # https://github.com/NixOS/nixpkgs/issues/57602
  xdg.portal = {
    enable = true;
    wlr.enable = true;
  };
}
