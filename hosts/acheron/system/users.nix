{ config, myStateDirectory, ... }:
let
  inherit (import ../../modules/system/users/lib.nix { inherit myStateDirectory; }) sopsUserPassword userHomeDirectory;
in
{
  sops.secrets = {
    ether = sopsUserPassword ./users.yaml;
    root = sopsUserPassword ./users.yaml;
    untrusted = sopsUserPassword ./users.yaml;
    work = sopsUserPassword ./users.yaml;
  };

  users.users.work = {
    isNormalUser = true;
    hashedPasswordFile = config.sops.secrets.work.path;
    home = "${userHomeDirectory}/work";
  };
}
