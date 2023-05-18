{ config, myLib, myPublicKey, myStateDirectory, mySystemDirectory, pkgs, ... }:
let
  inherit (myLib) mount state;
  userHomeDirectory = "${myStateDirectory}/users";
in
{
  fileSystems = {
    # This system boots via EFI, see the boot loader configuration below.
    # The evaluation will fail if fileSystems."/boot/efi".device is left unset.
    "/boot/efi".fsType = "vfat";
    # Don't do unncessary things (I'm ignoring systemd is configured to wipe
    # this directory less frequently). Can't use systemd.tmpfiles.rules without
    # creating a conflict.
    "/var/tmp" = mount.bind {
      device = "/tmp";
      depends = [ (assert builtins.hasAttr "/tmp" config.fileSystems; "/tmp") ];
    };
  } // state.binds [
    # Where systemd store persistent timers.
    "/var/lib/systemd/timers"
    # Log files.
    "/var/log"
  ];
  # Journal entries are keyed by machine ID. Note it's considered confidential:
  # https://www.freedesktop.org/software/systemd/man/machine-id.html
  # This symlink is created earlier than systemd.tmpfiles and is suitable for
  # the early init.
  environment.etc.machine-id.source = "${mySystemDirectory}/etc/machine-id";

  # Current GRUB version needs to be patched to find the LUKS 2 header.
  nixpkgs.overlays = [
    # https://discourse.nixos.org/t/is-there-grub-patched-for-booting-from-partition-encrypted-with-luks2/18398
    (final: parent: {
      grub2 = parent.grub2.overrideAttrs (old: {
        patches = old.patches ++ [ ./grub2_luks2.patch ];
      });
    })
  ];

  environment.systemPackages = with pkgs; [
    efibootmgr  # For editing EFI boot entries.
    dmidecode
    pciutils
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
  # Use systemd in the initrd. Be wary, not all features are ready yet:
  # https://github.com/NixOS/nixpkgs/projects/51
  boot.initrd.systemd.enable = true;

  systemd.tmpfiles.rules = [ "d ${userHomeDirectory} 750 root users - -" ];
  sops.secrets =
    let
      sopsSecret.sopsFile = ./system.yaml;
      sopsUserPassword = sopsSecret // { neededForUsers = true; };
    in
      {
        root = sopsUserPassword;
        ether = sopsUserPassword;
        untrusted = sopsUserPassword;
      };
  users = {
    mutableUsers = false;
    users = {
      root = {
        passwordFile = config.sops.secrets.root.path;
        openssh.authorizedKeys.keys = [ myPublicKey ];
      };
      ether = {
        isNormalUser = true;
        passwordFile = config.sops.secrets.ether.path;
        openssh.authorizedKeys.keys = [ myPublicKey ];
        home = "${userHomeDirectory}/ether";
        extraGroups = [ "wheel" ];
      };
      untrusted = {
        isNormalUser = true;
        passwordFile = config.sops.secrets.untrusted.path;
        home = "${userHomeDirectory}/untrusted";
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
  boot.tmp.useTmpfs = false;
}
