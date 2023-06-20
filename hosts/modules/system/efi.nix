{ pkgs, ... }: {
  # This system boots via EFI, see the boot loader configuration below.
  # The evaluation will fail if fileSystems."/boot/efi".device is left unset.
  fileSystems."/boot/efi".fsType = "vfat";

  # Current GRUB version needs to be patched to find the LUKS 2 header.
  nixpkgs.overlays = [
    # https://discourse.nixos.org/t/is-there-grub-patched-for-booting-from-partition-encrypted-with-luks2/18398
    (final: parent: {
      grub2 = parent.grub2.overrideAttrs (old: {
        patches = old.patches ++ [ ./grub2_luks2.patch ];
      });
    })
  ];

  boot.loader = {
    efi = {
      canTouchEfiVariables = true;
      efiSysMountPoint = "/boot/efi";
    };
    grub = {
      enable = true;
      device = "nodev";  # No BIOS.
      efiSupport = true;
      enableCryptodisk = true;
    };
  };

  environment.systemPackages = with pkgs; [
    efibootmgr  # For editing EFI boot entries.
  ];
}
