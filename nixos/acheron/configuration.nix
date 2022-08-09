{ pkgs, ... }:
{
  imports = [
    ../common/android.nix
    ../common/antivirus.nix
    ../common/audio.nix
    ../common/locale.nix
    ../common/monitoring.nix
    ../common/networking.nix
    ../common/nix.nix
    ../common/packages.nix
    ../common/system.nix
    ../common/virtualisation.nix
    ../common/vpn.nix
    ../common/wayland.nix
    ../common/yubikey.nix
    ./networking.nix
    ./system.nix
    ./wayland.nix
  ];

  networking.hostName = "acheron";
  time.timeZone = "Europe/Zurich";

  # This value determines the NixOS release from which the default
  # settings for stateful data, like file locations and database versions
  # on your system were taken. It‘s perfectly fine and recommended to leave
  # this value at the release version of the first install of this system.
  # Before changing this value read the documentation for this option
  # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
  system.stateVersion = "22.05"; # Did you read the comment?
}
