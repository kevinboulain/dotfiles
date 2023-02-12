# The evaluation will fail is these options are left unset:
#  fileSystems."/boot/efi".device
#  boot.initrd.luks.devices.root.device
{ config, efiDevice, pkgs, rootDevice, ... }:
{
  # This system uses LVM 2 on top of LUKS 2. GRUB needs to be patched to find
  # the header.
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

  # Using a tmpfs can cause issues with large builds:
  # https://github.com/NixOS/nixpkgs/issues/54707
  # It's however not as trivial as setting TMPDIR for nix-daemon: root doesn't
  # appear to use the nix-daemon and a 'sudo nixos-rebuild boot' after a
  # 'nixos-rebuild build` as user can actually result in a rebuild.
  # boot.tmpOnTmpfs = true;

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

  # The file system type must be specified to ensure modules are available.
  fileSystems."/".device = assert config.fileSystems."/".fsType != "auto"; "/dev/system/root";
  swapDevices = [ { device = "/dev/system/swap"; } ];
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

  # Prevent anyone not in the wheel group to run sudo.
  security.sudo.execWheelOnly = true;
}
