{ lib, myLib, pkgs, ... }:
let
  inherit (myLib) mount;
in
{
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
