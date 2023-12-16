{ myStateDirectory, ... }:
let
  inherit (import ../../modules/system/users/lib.nix { inherit myStateDirectory; }) sopsUserPassword;
in
{
  sops.secrets = {
    root = sopsUserPassword ./default.yaml;
    meshPrivateKey.sopsFile = ./default.yaml;
  };

  my.mesh = {
    endpoint = "[2603:c027:c000:5500:1806:951:9869:991a]:51820";
    publicKey = "IUkKSN4TZCO7Y4to5i6L/n1CEyH6GIWlztPNoGhgenc=";
  };
}
