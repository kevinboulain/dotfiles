{ config, myPublicKey, myStateDirectory, ... }:
let
  inherit (import ./lib.nix { inherit myStateDirectory; }) sopsUserPassword userHomeDirectory;
in
{
  sops.secrets.ether = sopsUserPassword;

  users.users.ether = {
    isNormalUser = true;
    passwordFile = config.sops.secrets.ether.path;
    openssh.authorizedKeys.keys = [ myPublicKey ];
    home = "${userHomeDirectory}/ether";
    extraGroups = [ "wheel" ];
  };
}
