{ pkgs, ... }:
{
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

  environment.systemPackages = with pkgs; [
    pavucontrol
  ];
}
