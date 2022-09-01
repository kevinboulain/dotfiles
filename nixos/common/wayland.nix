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
    wl-clipboard
  ];
  # For VLC (https://github.com/NixOS/nixpkgs/pull/58588/files).
  networking.firewall.allowedTCPPorts = [ 8010 ];

  # For screensharing (via WebRTC).
  # https://discourse.nixos.org/t/some-loose-ends-for-sway-on-nixos-which-we-should-fix/17728
  # https://github.com/NixOS/nixpkgs/issues/57602
  xdg.portal = {
    enable = true;
    wlr.enable = true;
  };
}
