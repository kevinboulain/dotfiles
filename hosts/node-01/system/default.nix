{ myLib, ... }:
let
  inherit (myLib) mount;
in
{
  imports = [ ./users.nix ];

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
    initrd.availableKernelModules = [ "virtio_pci" "virtio_scsi" ];
    kernelParams = [ "console=ttyS0,115200" ];
  };
}
