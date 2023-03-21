{ config, lib, ... }:
with lib;
let
  cfg = config.boot.rescue;
in
{
  options.boot.rescue = {
    isos = mkOption {
      type = types.listOf types.nonEmptyStr;
      default = [];
    };
  };

  config = {
    # https://www.supergrubdisk.org/wiki/Loopback.cfg
    boot.loader.grub.extraEntries =
      let
        # Impure and not authorized in pure flakes, too bad.
        # isos = builtins.attrNames (
        #   lib.filterAttrs (n: v: v == "regular" && (builtins.match ".*\\.iso" n != null))
        #     (builtins.readDir /boot/rescue));
        uuid = last (builtins.match "/dev/disk/by-uuid/(.*)" config.fileSystems."/boot/rescue".device);
      in
        ''
          submenu "ISOs" --class submenu {
            search --set=rescue --fs-uuid  ${uuid}
        '' + (lib.strings.concatMapStringsSep "\n" (iso: ''
            menuentry "${iso}" {
              loopback loop ($rescue)/${iso}
              root=(loop)
              configfile /boot/grub/loopback.cfg
              loopback --delete loop
           }
        '') cfg.isos) + ''
          }
        '';
  };
}
