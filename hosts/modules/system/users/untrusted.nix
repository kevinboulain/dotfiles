{ config, myStateDirectory, ... }:
let
  inherit (import ./lib.nix { inherit myStateDirectory; }) userHomeDirectory;
in
{
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
