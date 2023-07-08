{ config, lib, ... }: {
  # https://www.supergrubdisk.org/wiki/Loopback.cfg
  boot.loader.grub.extraEntries =
    let
      # Impure and not authorized in pure flakes, too bad.
      # isos = builtins.attrNames (
      #   lib.filterAttrs (n: v: v == "regular" && (builtins.match ".*\\.iso" n != null))
      #     (builtins.readDir /boot/rescue));
      uuid = lib.last (builtins.match "/dev/disk/by-uuid/(.*)" config.fileSystems."/boot/rescue".device);
    in
      ''
        submenu "ISOs" --class submenu {
          search --set=rescue --fs-uuid "${uuid}"
          insmod regexp  # Also for glob expansion.
          for path in ($rescue)/*.iso; do
            regexp --set=name "\($rescue\)(/.*)" "$path"
            menuentry "$name" "$path" {
              loopback loop "$2"
              root=(loop)
              iso_path="$1"
              export iso_path
              configfile /boot/grub/loopback.cfg
              loopback --delete loop
            }
          done
        }
      '';
}
