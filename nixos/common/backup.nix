{ config, lib, pkgs, ... }:
with lib;
let
  cfg = config.services.backup;
  borgDirectory = "/srv/secrets/borg";
in
{
  imports = [ ./snapshot.nix ];

  options.services.backup = {
    user = mkOption {
      type = types.nonEmptyStr;
      default = "";
    };
    host = mkOption {
      type = types.nonEmptyStr;
      default = "";
    };
    publicKey = mkOption {
      type = types.nonEmptyStr;
      default = "";
    };
  };

  config = {
    services.borgbackup.jobs.snapshot = {
      # Ideally, the repository should be set up in append-only mode:
      # https://borgbackup.readthedocs.io/en/stable/usage/notes.html#append-only-mode-forbid-compaction
      # But see https://github.com/borgbackup/borg/issues/3504.
      repo = "${cfg.user}@${cfg.host}:${config.networking.hostName}";
      doInit = false;
      encryption = {
        mode = "keyfile";
        # I don't believe there's a point in protecting the keyfile.
        passphrase = "";
      };

      startAt = [ "daily" ];
      persistentTimer = true;

      dateFormat = "+%F-%H-%M-%S";
      archiveBaseName = null;
      compression = "zstd";

      extraArgs = "--debug";
      readWritePaths = [ borgDirectory ];
      environment = {
        BORG_BASE_DIR = borgDirectory;
        # Reuse the host's SSH key, that's one less secret to manage. See also
        # https://restic.readthedocs.io/en/latest/030_preparing_a_new_repo.html#sftp
        BORG_RSH = "ssh -v -o StrictHostKeyChecking=yes -o UserKnownHostsFile=${pkgs.writeText "${cfg.host}" "${cfg.host} ${cfg.publicKey}"} -o ServerAliveInterval=60 -o ServerAliveCountMax=240 -i ${(builtins.elemAt config.services.openssh.hostKeys 0).path}";
      };

      paths = map (subvolume: "${subvolume}/.snapshots/latest") config.services.snapshot.subvolumes;
      # https://borgbackup.readthedocs.io/en/stable/usage/help.html?highlight=pattern#borg-help-patterns
      # > If you give /absolute/ as root, the paths going into the matcher will
      # > look relative like absolute/…/file.ext
      exclude =
        # There is no recursion since each snapshot is a separate filesystem but
        # that prevents from copying empty directories.
        (map (subvolume: strings.removePrefix "/" "${subvolume}/.snapshots/latest/.snapshots") config.services.snapshot.subvolumes) ++ [
          # TODO
        ];
    };
    systemd.tmpfiles.rules = [ "d ${borgDirectory} 700 root root - -" ];
  };
}
