{ pkgs, ... }:
{
  environment.systemPackages = with pkgs; [
    firefox-wayland
    grim
    light
    mpv
    pavucontrol
    slurp
    wl-clipboard
    zathura
  ];

  # https://nixos.wiki/wiki/Actkbd
  # Global keybindings without a desktop environment.
  services.actkbd = {
    enable = true;
    bindings = [
      { keys = [ 224 ]; events = [ "rep" ]; command = "/run/current-system/sw/bin/light -U 1"; }
      { keys = [ 225 ]; events = [ "rep" ]; command = "/run/current-system/sw/bin/light -A 1"; }
    ];
  };

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

  # For screensharing (via WebRTC).
  # https://discourse.nixos.org/t/some-loose-ends-for-sway-on-nixos-which-we-should-fix/17728
  # https://github.com/NixOS/nixpkgs/issues/57602
  xdg.portal = {
    enable = true;
    wlr.enable = true;
  };

  hardware.bluetooth = {
    enable = true;
    package = pkgs.bluez5-experimental;
    settings = {
      # Experimental D-Bus interface (e.g.: integration with UPower).
      General.Experimental = true;
      # To save some power.
      Policy.AutoEnable = false;
    };
  };
  # https://github.com/NixOS/nixpkgs/issues/170573
  systemd.tmpfiles.rules = [
    "d /var/lib/bluetooth 700 root root - -"
  ];
  systemd.targets.bluetooth.after = ["systemd-tmpfiles-setup.service"];

  # Handle media key on bluetooth headsets.
  # https://wiki.archlinux.org/title/MPRIS#Bluetooth
  systemd.user.services.mpris-proxy = {
    serviceConfig.ExecStart = "${pkgs.bluez}/bin/mpris-proxy";
    wantedBy = [ "default.target" ];
  };

  # https://nixos.wiki/wiki/PipeWire
  security.rtkit.enable = true;  # For PipeWire to be able to set realtime priority.
  services.pipewire = {
    enable = true;
    alsa.enable = true;
    pulse.enable = true;
  };
}
