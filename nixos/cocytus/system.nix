{ ... }:
{
  fileSystems."/boot/efi".device = "/dev/disk/by-uuid/D3B4-708A";
  boot = {
    kernelParams = [ "fbcon=rotate:1" ];
    initrd.luks.devices.root.device = "/dev/disk/by-uuid/b5be843e-874a-4e6f-84d1-4eab48a19c25";
  };
}