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
    endpoint = "[2603:c027:c000:5500:e12f:8356:2232:fb82]:51820";
    publicKey = "gR5D9DwaYHWi/bGyyJiWdyQgyysYZK1GHCctY7MZeBQ=";
  };
}
