{ lib, myLib, pkgs, ... }:
let
  inherit (myLib) mount;
in
{
  fileSystems = {
    "/boot".fsType = "ext4";
    "/boot/efi".depends = [ "/boot" ];
    "/swap" = mount.btrfs {
      device = "/dev/mapper/root";
      subvolume = "system/swap";
    };
  };

  swapDevices = [{ device = "/swap/swap"; }];

  boot = {
    kernelPackages = pkgs.linuxPackages;
    kernelParams = [ "fbcon=rotate:1" ];
  };
}
