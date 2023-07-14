{ pkgs, ... }: {
  # This system boots via EFI, see the boot loader configuration below.
  # The evaluation will fail if fileSystems."/boot/efi".device is left unset.
  fileSystems."/boot/efi".fsType = "vfat";

  boot.loader = {
    efi = {
      canTouchEfiVariables = true;
      efiSysMountPoint = "/boot/efi";
    };
    grub = {
      # LUKS 2 support was introduced in GRUB 2.12.
      enable = assert builtins.compareVersions pkgs.grub2.version "2.12" >= 0; true;
      device = "nodev";  # No BIOS.
      efiSupport = true;
      enableCryptodisk = true;
    };
  };

  environment.systemPackages = with pkgs; [
    efibootmgr  # For editing EFI boot entries.
  ];
}
