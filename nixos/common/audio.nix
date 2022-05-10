{ pkgs, ... }:
{
  # https://nixos.wiki/wiki/PipeWire
  hardware.bluetooth.enable = true;
  security.rtkit.enable = true;  # For PipeWire to be able to set realtime priority.
  services.pipewire = {
    enable = true;
    alsa.enable = true;
    pulse.enable = true;
  };

  environment.systemPackages = with pkgs; [
    pavucontrol
  ];

  # https://github.com/NixOS/nixpkgs/issues/170573
  systemd.tmpfiles.rules = [
    "d /var/lib/bluetooth 700 root root - -"
  ];
  systemd.targets.bluetooth.after = ["systemd-tmpfiles-setup.service"];
}
