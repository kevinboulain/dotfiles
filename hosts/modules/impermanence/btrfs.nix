{
  lib,
  myHostsLib,
  myStateDirectory,
  mySystemDirectory,
  ...
}:
let
  inherit (myHostsLib) mount;
in
{
  fileSystems = {
    "/" = mount.tmpfs {
      options = [
        "X-mount.mode=755"
        # Systemd mounts the sysroot before resuming from hiberation:
        # https://github.com/NixOS/nixpkgs/issues/213122
        "x-systemd.after=local-fs-pre.target"
      ];
    };
    "/nix" = mount.btrfs {
      device = "/dev/mapper/root";
      subvolume = "system/nix";
      options = [ "noatime" ];
    };
    "/tmp" = mount.btrfs {
      device = "/dev/mapper/root";
      subvolume = "system/tmp";
      # Stolen from
      # https://github.com/NixOS/nixpkgs/blob/nixos-22.11/nixos/modules/system/boot/tmp.nix
      options = [
        "X-mount.mode=1777"
        "nodev"
        "nosuid"
        "strictatime"
      ];
    };
    "${myStateDirectory}" = mount.btrfs {
      device = "/dev/mapper/root";
      subvolume = "state";
      # sops-nix uses the host SSH key to decrypt secrets, including passwords
      # that are set at startup.
      neededForBoot =
        assert lib.strings.hasPrefix "${myStateDirectory}/" mySystemDirectory;
        true;
    };
  };

  services.snapshot.subvolumes = [ myStateDirectory ];
}
