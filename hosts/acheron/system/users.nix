{ myStateDirectory, ... }:
let
  inherit (import ../../modules/system/users/lib.nix { inherit myStateDirectory; }) sopsUserPassword;
in
{
  sops.secrets = {
    ether = sopsUserPassword ./users.yaml;
    root = sopsUserPassword ./users.yaml;
    untrusted = sopsUserPassword ./users.yaml;
  };
}
