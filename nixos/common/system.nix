arguments@{ config, lib, pkgs, ... }:
let
  inherit (import ./lib.nix arguments) mount;
  secretsDirectory = "/srv/secrets";
  userPasswordsDirectory = "${secretsDirectory}/users";
in
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

  # This system boots via EFI.
  # The evaluation will fail if fileSystems."/boot/efi".device is left unset.
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
  # Use systemd in the initrd. Be wary, not all features are ready yet:
  # https://github.com/NixOS/nixpkgs/projects/51
  boot.initrd.systemd.enable = true;

  systemd.tmpfiles.rules = with lib; [
    "d ${secretsDirectory} 700 root root - -"
    "d ${userPasswordsDirectory} 700 root root - -"
    "d /srv/users 750 root users - -"
  ];
  # users.users.*.passwordFile is applied at boot time and only a script
  # displays a warning when the file is missing.
  warnings = lib.optional (!builtins.pathExists "${userPasswordsDirectory}/root")
    "${userPasswordsDirectory}/root doesn't exist, defaulting to 'root'";
  users = {
    mutableUsers = false;
    users = {
      root =
        if (builtins.pathExists "${userPasswordsDirectory}/root")
        then { passwordFile = assert config.fileSystems."/srv".neededForBoot; "${userPasswordsDirectory}/root"; }
        else { initialPassword = "root"; };
      ether = {
        isNormalUser = true;
        passwordFile = "${userPasswordsDirectory}/ether";
        home = "/srv/users/ether";
        extraGroups = [ "wheel" ];
      };
      untrusted = {
        isNormalUser = true;
        passwordFile = "${userPasswordsDirectory}/untrusted";
        home = "/srv/users/untrusted";
        homeMode = "0750";
      };
    };
  };
  services.logind.extraConfig = ''
    KillOnlyUsers=untrusted
  '';

  # Prevent anyone not in the wheel group to run sudo.
  security.sudo.execWheelOnly = true;

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
  boot.tmpOnTmpfs = false;
  # Don't do unncessary things (I'm ignoring systemd is configured to wipe this
  # directory less frequently). Can't use systemd.tmpfiles.rules without
  # creating a conflict.
  fileSystems."/var/tmp" = mount.bind {
    device = "/tmp";
    depends = [ (assert builtins.hasAttr "/tmp" config.fileSystems; "/tmp") ];
  };
}
