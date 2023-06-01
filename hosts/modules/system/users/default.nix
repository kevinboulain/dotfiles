{ config, lib, myStateDirectory, ... }:
let
  inherit (import ./lib.nix { inherit myStateDirectory; }) userHomeDirectory;
in
{
  users.mutableUsers = false;
  systemd.tmpfiles.rules = lib.lists.optional
    (builtins.any
      (user: lib.strings.hasPrefix "${userHomeDirectory}/" user.home)
      (builtins.attrValues config.users.users))
    "d ${userHomeDirectory} 750 root users - -";
}
