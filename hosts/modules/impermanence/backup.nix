{
  config,
  lib,
  mySystemDirectory,
  pkgs,
  ...
}:
with lib;
{
  config = mkIf (config.services.snapshot.subvolumes != [ ]) {
    sops.secrets =
      let
        sopsSecret.sopsFile = ./backup.yaml;
      in
      {
        backupUser = sopsSecret // {
          key = "user";
        };
        backupHost = sopsSecret // {
          key = "host";
        };
        backupPublicKey = sopsSecret // {
          key = "publicKey";
        };
        backupExclude = sopsSecret // {
          key = "exclude";
        };
      };

    # Some ideas stolen from
    # https://github.com/NixOS/nixpkgs/blob/master/nixos/modules/services/backup/borgbackup.nix
    # I'm not reusing it because I'd like to encrypt some details.
    systemd.timers.backup = {
      after = [
        # TODO: this seems to be triggered before iwd gets an IP from the DHCP.
        "network-online.target"
      ];
      wants = [ "network-online.target" ];
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
    systemd.services.backup.serviceConfig =
      with pkgs;
      let
        borgBaseDir = "${mySystemDirectory}/var/lib/borg";
      in
      {
        # Oneshot would slow down the boot by a few seconds when the persistent
        # timer has been missed. Instead, only ensure the script can be
        # executed.
        Type = "exec";
        CPUSchedulingPolicy = "idle";
        IOSchedulingClass = "idle";
        ProtectSystem = "strict";
        PrivateTmp = true;
        ReadWritePaths = [ borgBaseDir ];
        ExecStart = writeShellScript "backup" ''
          set -euo pipefail
          export PATH="${
            makeBinPath [
              borgbackup
              coreutils
            ]
          }"

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
          #  rclone serve restic --stdio --append-only path/to/repository
          # isn't supported by my provider while
          #  borg serve --append-only --restrict-to-repository path/to/repository
          # is...
          # For some inspiration, see also
          # https://restic.readthedocs.io/en/latest/030_preparing_a_new_repo.html#sftp
          #
          # To set up a new repository, on a trusted machine:
          #  sudo BORG_RSH='ssh -vi ~$user/.ssh/$key' BORG_BASE_DIR=/srv/system/var/lib/borg borg init --encryption keyfile $user@$host:$hostname
          # No need for a passphrase since the keyfile will only be stored on
          # the trusted machine. Back it up and copy it to the machine to be
          # backed up:
          #  sudo mkdir -pm 0700 /srv/system/var/lib/borg/.config/borg/keys
          #  sudo chmod -R 0700 /srv/system/var/lib/borg
          #  sudo chmod 0600 /srv/system/var/lib/borg/.config/borg/keys/$key
          # Add the machine's public key to the (restricted) authorized keys on
          # the repository server:
          #  restrict,command="borg serve --append-only --restrict-to-repository $hostname" $pubkey
          # Start a backup:
          #  sudo systemctl start backup.service
          # And finally, verify the backup on the trusted machine:
          #  sudo BORG_RSH='ssh -i ~$user/.ssh/$key' BORG_BASE_DIR=/srv/system/var/lib/borg borg mount $user@$host:$hostname /mnt
          export BORG_RSH="ssh -v -o StrictHostKeyChecking=yes -o UserKnownHostsFile=$(printf '%q' "$public_key_file") -o ServerAliveInterval=60 -o ServerAliveCountMax=240 -i ${
            escapeShellArg (builtins.elemAt config.services.openssh.hostKeys 0).path
          }"
          export BORG_REPO="$user@$host:"${escapeShellArg config.networking.hostName}

          # Wait until the network is reachable: for some reason
          # network-online is reached before iwd can get an IP.
          declare -i pings=0
          while [ $pings -lt 10 ] && ! borg info; do
            sleep 30
            pings=$((pings+1))
          done

          archive=$(date '+%F-%H-%M-%S')
          # Regarding --exclude, there's actually no recursion since each
          # snapshot is a separate filesystem, it only prevents copying empty
          # directories.
          # https://borgbackup.readthedocs.io/en/stable/usage/help.html#borg-patterns
          # > If you give /absolute/ as root, the paths going into the matcher
          # > will look relative like absolute/…/file.ext
          borg create --debug --compression zstd \
            ${
              escapeShellArgs (
                map (path: "--exclude=${path}") (
                  map (
                    subvolume: strings.removePrefix "/" "${subvolume}/.snapshots/latest/.snapshots"
                  ) config.services.snapshot.subvolumes
                )
              )
            } \
            --exclude-from ${escapeShellArg config.sops.secrets.backupExclude.path} \
            --exclude-nodump \
            ::"$archive".failed \
            ${
              escapeShellArgs (
                map (subvolume: "${subvolume}/.snapshots/latest") config.services.snapshot.subvolumes
              )
            }
          borg rename --debug ::"$archive".failed "$archive"
        '';
      };
  };
}
