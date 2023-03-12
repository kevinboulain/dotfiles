{ config, lib, ... }:
let
  isos = builtins.attrNames (
    lib.filterAttrs (n: v: v == "regular" && (builtins.match ".*\\.iso" n != null))
      (builtins.readDir /boot/rescue));
in
{
  # https://www.supergrubdisk.org/wiki/Loopback.cfg
  boot.loader.grub.extraEntries = ''
    submenu "ISOs" --class submenu {
      search --set=rescue --fs-uuid  ${lib.last (builtins.match "/dev/disk/by-uuid/(.*)" config.fileSystems."/boot/rescue".device)}
  '' + (lib.strings.concatMapStringsSep "\n" (iso: ''
      menuentry "${iso}" {
        loopback loop ($rescue)/${iso}
        root=(loop)
        configfile /boot/grub/loopback.cfg
        loopback --delete loop
      }
    '') isos) + ''
    }
  '';
}
