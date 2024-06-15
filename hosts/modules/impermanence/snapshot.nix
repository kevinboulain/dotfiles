{
  config,
  lib,
  pkgs,
  ...
}:
with lib;
let
  inherit (config.services.snapshot) subvolumes;
in
{
  options.services.snapshot = {
    subvolumes = mkOption {
      type = types.listOf types.nonEmptyStr;
      default = [ ];
    };
  };

  config = {
    systemd.services.snapshot = {
      enable = true;
      # https://www.freedesktop.org/software/systemd/man/bootup.html
      after = [ "local-fs.target" ];
      before = [ "sysinit.target" ];
      wantedBy = [ "sysinit.target" ];
      unitConfig.DefaultDependencies = false;
      serviceConfig = with pkgs; {
        Type = "oneshot";
        RemainAfterExit = true;
        ExecStartPre =
          let
            script = writeShellScript "snapshot" ''
              set -euo pipefail
              [ "$#" -eq 2 ]
              export PATH="${
                makeBinPath [
                  btrfs-progs
                  coreutils
                  findutils
                  util-linux
                ]
              }"
              mkdir -p "$2"/latest
              # At most one snapshot per hour.
              name=$(date +'%F-%H')
              [ -d "$2"/"$name" ] || btrfs subvolume snapshot -r "$1" "$2"/"$name"
              # Symlinks (even as the target) aren't followed by either Borg not restic:
              # https://github.com/borgbackup/borg/issues/4737
              # https://github.com/restic/restic/issues/2564
              umount "$2"/latest || true
              mount --bind "$2"/"$name" "$2"/latest
              # Remove snapshots older than a week (but always keep three of
              # them, in case reboots are infrequent).
              # Use atime instead of mtime because it would be the filesystem's,
              # not the snapshot's.
              find "$2" -mindepth 1 -maxdepth 1 -atime +7 -print0 \
                | sort -z | head -zn -3 \
                | xargs -0I{} btrfs subvolume delete {}
            '';
          in
          map (
            subvolume:
            "${script} ${
              escapeShellArgs [
                subvolume
                "${subvolume}/.snapshots"
              ]
            }"
          ) subvolumes;
        ExecStart = "${coreutils}/bin/true";
      };
    };
  };
}
