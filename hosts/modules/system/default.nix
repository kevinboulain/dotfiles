{ config, myLib, mySystemDirectory, pkgs, ... }:
let
  inherit (myLib) mount state;
in
{
  fileSystems = {
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

  environment.systemPackages = with pkgs; [
    dmidecode
    pciutils
    usbutils
  ];

  # Use systemd in the initrd. Be wary, not all features are ready yet:
  # https://github.com/NixOS/nixpkgs/projects/51
  boot.initrd.systemd.enable = true;

  # Prevent anyone not in the wheel group to run sudo.
  security.sudo.execWheelOnly = true;

  # Using a tmpfs can cause issues with large builds:
  # https://github.com/NixOS/nixpkgs/issues/54707
  # It's however not as trivial as setting TMPDIR for nix-daemon: root doesn't
  # appear to use the nix-daemon and a 'sudo nixos-rebuild boot' after a
  # 'nixos-rebuild build` as user can actually result in a rebuild.
  boot.tmp.useTmpfs = false;
}
