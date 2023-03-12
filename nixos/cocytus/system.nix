arguments@{ lib, pkgs, ... }:
let
  inherit (import ../common/lib.nix arguments) mount;
  efiDevice = "/dev/disk/by-uuid/D43D-D268";
  bootDevice = "/dev/disk/by-uuid/a6068654-68d4-4389-8197-becf0f9ecec8";
  luksRootDevice = "/dev/disk/by-uuid/3e0e069f-87c3-474c-9aee-b2aa1239a0a4";
in
{
  boot.initrd.luks.devices.root.device = luksRootDevice;

  fileSystems = {
    "/" = mount.tmpfs {
      options = [
        "mode=755"
        # Systemd mounts the sysroot before resuming from hiberation:
        # https://github.com/NixOS/nixpkgs/issues/213122
        "x-systemd.after=local-fs-pre.target"
      ];
    };
    "/boot" = {
      device = bootDevice;
      fsType = "ext4";
    };
    "/boot/efi" = {
      device = efiDevice;
      depends = [ "/boot" ];
    };
    "/nix" = mount.btrfs {
      subvolume = "system/nix";
      options = [ "noatime" ];
    };
    "/swap" = mount.btrfs { subvolume = "system/swap"; };
    "/tmp" = mount.btrfs {
      subvolume = "system/tmp";
      # Stolen from
      # https://github.com/NixOS/nixpkgs/blob/nixos-22.11/nixos/modules/system/boot/tmp.nix
      options = [ "mode=1777" "nodev" "nosuid" "strictatime" ];
    };
    "/srv" = mount.btrfs {
      subvolume = "state";
      # /srv/secrets hosts user passwords and are read at startup.
      neededForBoot = true;
    };
  };

  services.snapshot.subvolumes = [ "/srv" ];

  swapDevices = [{ device = "/swap/swap"; }];

  boot = {
    kernelPackages = pkgs.linuxPackages;
    kernelParams = [ "fbcon=rotate:1" ];
  };

  # Enable SSH in the initrd.
  boot.initrd.network = {
    enable = true;
    # Evaluation will fail without:
    #  - boot.initrd.network.ssh.authorizedKeys (defaults to
    #    users.users.root.openssh.authorizedKeys.keys),
    #  - boot.initrd.network.ssh.hostKeys.
    # Once SSH'd in:
    #  read -s password && echo "$password" > /crypt-ramfs/passphrase
    ssh.enable = true;
  };
  boot.initrd.availableKernelModules = [ "r8169" ];

  # TODO: a bunch of workarounds...
  boot.initrd.network.udhcpc.extraArgs = [
    # This isn't perfect: after the 2 minutes, udhcpc will exit (unless forked
    # in the background with --background but that doesn't seem expected by the
    # script). See also https://github.com/NixOS/nixpkgs/issues/98741
    "--timeout" "6"
    "--retries" "20"
  ];
  # SSH is with systemd stage 1 isn't supported yet:
  # https://github.com/NixOS/nixpkgs/pull/169116
  boot.initrd.systemd.enable = lib.mkForce false;
  # The Nix modules are kinda leaky abstraction-wise, and doesn't work with
  # systemd-networkd: https://github.com/NixOS/nixpkgs/issues/157034
  networking = {
    dhcpcd.enable = false;
    useDHCP = lib.mkForce true;
  };
}
