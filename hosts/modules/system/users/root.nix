{ config, myPublicKey, myStateDirectory, ... }:
let
  inherit (import ./lib.nix { inherit myStateDirectory; }) sopsUserPassword;
in
{
  sops.secrets.root = sopsUserPassword;

  users.users.root = {
    passwordFile = config.sops.secrets.root.path;
    openssh.authorizedKeys.keys = [ myPublicKey ];
  };
}
