{ config, lib, ... }:
{
  mount = rec {
    bind = { device, depends ? [] }: {
      inherit device depends;
      fsType = "none";
      options = [ "bind" ];
    };

    # install-grub.pl is inflexible and really wants one device per mountpoint
    # (for at least /boot) and resorting to bind mounts or X-mount.subdir leads
    # nowhere because the GRUB entries will be missing part of the path (e.g.:
    # ($device)/store instead of ($device)/nix/store).
    # Suck it up and create Btrfs subvolumes, the only downside is that kernels
    # and initrds will (unnecessarily?) be copied from /nix/store to
    # /boot/kernels.
    # Note that some options are global to the filesystem:
    # https://btrfs.readthedocs.io/en/latest/Administration.html#mount-options
    # https://btrfs.readthedocs.io/en/latest/Subvolumes.html#mount-options
    btrfs = { subvolume, options ? [], neededForBoot ? false }: {
      device = "/dev/mapper/root";
      fsType = "btrfs";
      options = [ "subvol=${subvolume}" "compress=zstd" ] ++ options;
      inherit neededForBoot;
    };

    tmpfs = { options? [] }: {
      device = "tmpfs";
      fsType = "tmpfs";
      inherit options;
    };

    systemBind = path: bind {
      device = "/srv/system${path}";
      depends = [ (assert builtins.hasAttr "/srv" config.fileSystems; "/srv") ];
    };
    systemBinds = paths: lib.attrsets.genAttrs paths systemBind;
  };
}
