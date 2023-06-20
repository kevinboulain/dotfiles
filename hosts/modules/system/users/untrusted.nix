{ config, myStateDirectory, ... }:
let
  inherit (import ./lib.nix { inherit myStateDirectory; }) sopsUserPassword userHomeDirectory;
in
{
  sops.secrets.untrusted = sopsUserPassword;

  users.users.untrusted = {
    isNormalUser = true;
    passwordFile = config.sops.secrets.untrusted.path;
    home = "${userHomeDirectory}/untrusted";
    homeMode = "0750";
  };

  services.logind.extraConfig = ''
    KillOnlyUsers=untrusted
  '';
}
