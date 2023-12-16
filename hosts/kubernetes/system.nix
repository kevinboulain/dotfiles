{ myLib, ... }:
let
  inherit (myLib) mount;
in
{
  fileSystems = {
    "/boot".fsType = "ext4";
    "/boot/efi".depends = [ "/boot" ];
  };

  boot = {
    initrd.availableKernelModules = [ "virtio_pci" "virtio_scsi" ];
    kernelParams = [ "console=ttyS0,115200" ];
  };
}
