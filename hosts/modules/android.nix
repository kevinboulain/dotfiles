{ ... }:
{
  programs.adb.enable = true;
  users.users.ether.extraGroups = [ "adbusers" ];
}
