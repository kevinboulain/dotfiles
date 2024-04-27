{ myHostsLib, ... }:
let
  inherit (myHostsLib) mount;
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
    initrd.availableKernelModules = [
      # Otherwise there's no console.
      "virtio_gpu"
      # Otherwise there's no disk.
      "virtio_scsi"
    ];
    kernelParams = [ "console=tty1" ];
  };
}
