{ config, lib, mySystemDirectory, pkgs, ... }:
with lib;
{
  config = mkIf (config.services.snapshot.subvolumes != []) {
    sops.secrets =
      let
        sopsSecret.sopsFile = ./backup.yaml;
      in
        {
          backupUser = sopsSecret // { key = "user"; };
          backupHost = sopsSecret // { key = "host"; };
          backupPublicKey = sopsSecret // { key = "publicKey"; };
          backupExclude = sopsSecret // { key = "exclude"; };
        };

    # Some ideas stolen from
    # https://github.com/NixOS/nixpkgs/blob/master/nixos/modules/services/backup/borgbackup.nix
    # I'm not reusing it because I'd like to encrypt some details.
    systemd.timers.backup = {
      after = [
        # TODO: this seems to be triggered before iwd gets an IP from the DHCP.
        "network-online.target"
      ];
      wantedBy = [ "timers.target" ];
      timerConfig = {
        Persistent = true;
        # Note that there's one limitation to keep in mind: the snapshot is made
        # at boot time, before all services are started so that we can get a
        # truly consistent view of the file system. That means performing daily
        # backups doesn't necessarily imply grabbing new changes daily, only a
        # reboot will ensure that. For now, and for simplicity, I'm okay with
        # this limitation.
        OnCalendar = "daily";
      };
    };
    systemd.services.backup.serviceConfig = with pkgs;
      let
        borgBaseDir = "${mySystemDirectory}/var/lib/borg";
      in
        {
          Type = "oneshot";
          CPUSchedulingPolicy = "idle";
          IOSchedulingClass = "idle";
          ProtectSystem = "strict";
          PrivateTmp = true;
          ReadWritePaths = [ borgBaseDir ];
          ExecStart = writeShellScript "backup" ''
            set -euo pipefail
            export PATH="${makeBinPath [ borgbackup coreutils ]}"

            user=$(<${escapeShellArg config.sops.secrets.backupUser.path})
            host=$(<${escapeShellArg config.sops.secrets.backupHost.path})
            public_key=$(<${escapeShellArg config.sops.secrets.backupPublicKey.path})
            public_key_file=$(mktemp)
            printf '%s %s\n' "$host" "$public_key" > "$public_key_file"

            # .config/borg hosts two important things:
            #  - the keyfile (so no secret is stored on the server, should be
            #    safely stored somewhere else unless losing the backup is
            #    acceptable),
            #  - the nonce (to avoid attacks on AES CTR).
            export BORG_BASE_DIR=${escapeShellArg borgBaseDir}
            # Reuse the host's SSH key, that's one less secret to manage.
            export BORG_PASSPHRASE=
            # Ideally, the server would enforce append-only mode:
            # https://borgbackup.readthedocs.io/en/stable/usage/notes.html#append-only-mode-forbid-compaction
            # But see https://github.com/borgbackup/borg/issues/1772
            # I kinda wish I could use restic instead but
            #   rclone serve restic --stdio --append-only path/to/repository
            # isn't supported by my provider while
            #   borg serve --append-only --restrict-to-repository path/to/repository
            # is...
            # For some inspiration, see also
            # https://restic.readthedocs.io/en/latest/030_preparing_a_new_repo.html#sftp
            export BORG_RSH="ssh -v -o StrictHostKeyChecking=yes -o UserKnownHostsFile=$(printf '%q' "$public_key_file") -o ServerAliveInterval=60 -o ServerAliveCountMax=240 -i ${escapeShellArg (builtins.elemAt config.services.openssh.hostKeys 0).path}"
            export BORG_REPO="$user@$host:"${escapeShellArg config.networking.hostName}

            archive=$(date '+%F-%H-%M-%S')
            # Regarding --exclude, there's actually no recursion since each
            # snapshot is a separate filesystem, it only prevents copying empty
            # directories.
            # https://borgbackup.readthedocs.io/en/stable/usage/help.html#borg-patterns
            # > If you give /absolute/ as root, the paths going into the matcher
            # > will look relative like absolute/…/file.ext
            borg create --debug --compression zstd \
              ${escapeShellArgs
                (map (path: "--exclude=${path}")
                  (map (subvolume: strings.removePrefix "/" "${subvolume}/.snapshots/latest/.snapshots") config.services.snapshot.subvolumes))} \
              --exclude-from ${escapeShellArg config.sops.secrets.backupExclude.path} \
              --exclude-nodump \
              ::"$archive".failed \
              ${escapeShellArgs (map (subvolume: "${subvolume}/.snapshots/latest") config.services.snapshot.subvolumes)}
            borg rename --debug ::"$archive".failed "$archive"
          '';
        };
  };
}
