arguments@{ lib, pkgs, ... }:
{
  imports = [
    ../common/android.nix
    ../common/antivirus.nix
    ../common/desktop.nix
    ../common/locale.nix
    ../common/monitoring.nix
    (import ../common/networking.nix (arguments // { withVPN = true; }))
    ../common/nix.nix
    ../common/packages.nix
    ../common/steam.nix
    ../common/system.nix
    ../common/yubikey.nix
    ./desktop.nix
    ./networking.nix
    ./system.nix
  ] ++ lib.optional (builtins.pathExists ./local.nix) ./local.nix;

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
