# The evaluation will fail is these options are left unset:
#  fileSystems."/boot/efi".device
#  boot.initrd.luks.devices.root.device
{ pkgs, efiDevice, rootDevice, ... }:
{
  nixpkgs.overlays = [
    (import ../common/overlays/grub2)
  ];

  environment.systemPackages = with pkgs; [
    efibootmgr  # For editing EFI boot entries.
    dmidecode
    pciutils
  ];

  # Enable non-free firmware.
  nixpkgs.config.allowUnfree = true;
  hardware = {
    enableRedistributableFirmware = true;
    cpu.intel.updateMicrocode = true;
  };

  boot.tmpOnTmpfs = true;  # Not true by default, like cleanTmpDir.

  # This system boots via EFI.
  fileSystems."/boot/efi".fsType = "vfat";
  boot.loader = {
    efi = {
      canTouchEfiVariables = true;
      efiSysMountPoint = "/boot/efi";
    };
    grub = {
      enable = true;
      version = 2;
      device = "nodev";  # No BIOS.
      efiSupport = true;
      enableCryptodisk = true;
    };
  };

  # This system uses LVM 2 on top of LUKS 2. GRUB needs to be patched to find
  # the header.
  fileSystems."/" = {
    device = "/dev/system/root";
    fsType = "ext4";
  };
  swapDevices = [
    { device = "/dev/system/swap"; }
  ];
  boot.initrd = {
    systemd.enable = true;  # Use systemd in the initrd.
    kernelModules = [ "dm-snapshot" ];
    luks.devices = {
      root = {
        keyFile = "/root.key";
        preLVM = true;
      };
    };
    # A LUKS key to avoid entering the passphrase twice. This is safe as long
    # as the permissions aren't too wide (the resulting initrd doesn't end up
    # in the store but /boot/kernels).
    secrets."/root.key" = "/etc/secrets/root.key";
  };

  users.users = {
    ether = {
      isNormalUser = true;
      home = "/home/ether";
      extraGroups = [ "wheel" ];
    };
    untrusted = {
      isNormalUser = true;
      home = "/home/untrusted";
      homeMode = "0750";
    };
  };
  services.logind.extraConfig = ''
    KillOnlyUsers=untrusted
  '';
}
