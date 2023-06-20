{ lib, mySystemDirectory, ... }: rec {
  mount = {
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
    btrfs = { device, subvolume, options ? [], neededForBoot ? false }: {
      inherit device neededForBoot;
      fsType = "btrfs";
      options = [ "subvol=${subvolume}" "compress=zstd" ] ++ options;
    };

    tmpfs = { options? [] }: {
      inherit options;
      device = "tmpfs";
      fsType = "tmpfs";
    };
  };

  state = rec {
    bind = path: mount.bind {
      device = "${mySystemDirectory}${path}";
      # I previously had a depends here but that shouldn't be necessary with
      # systemd's resolution (and that leads to less spaghetti: ideally
      # config.fileSystems should be checked for myStateDirectory).
    };
    binds = paths: lib.attrsets.genAttrs paths bind;
  };
}
