{ config, myHostsLib, pkgs, ... }:
let
  inherit (myHostsLib) state;
in
{
  # https://nixos.wiki/wiki/Actkbd
  # Global keybindings without a desktop environment.
  services.actkbd = {
    enable = true;
    bindings = [
      { keys = [ 224 ]; events = [ "rep" ]; command = "${pkgs.light}/bin/light -U 1"; }
      { keys = [ 225 ]; events = [ "rep" ]; command = "${pkgs.light}/bin/light -A 1"; }
    ];
  };

  # https://nixos.wiki/wiki/Sway
  programs.sway = {
    enable = true;
    wrapperFeatures.gtk = true;
    extraPackages = with pkgs; [ swaylock ];
  };

  # For screensharing (via WebRTC).
  # https://discourse.nixos.org/t/some-loose-ends-for-sway-on-nixos-which-we-should-fix/17728
  # https://github.com/NixOS/nixpkgs/issues/57602
  xdg.portal.enable = true;
  # TODO: xdg-document-portal and xdg-permission-store often prevent shutdown.

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
  fileSystems = state.binds [
    # Stores paired devices.
    # See also https://github.com/NixOS/nixpkgs/issues/170573.
    # TODO? permissions
    "/var/lib/bluetooth"
    # Stores display backlight.
    "/var/lib/systemd/backlight"
  ];

  # https://nixos.wiki/wiki/PipeWire
  security.rtkit.enable = true;  # For PipeWire to be able to set realtime priority.
  services.pipewire = {
    enable = true;
    alsa.enable = true;
    pulse.enable = true;
  };

  # Requires Avahi for printer discovery:
  # https://github.com/apple/cups/issues/5452
  services.printing.enable = assert config.services.avahi.enable; true;
}
