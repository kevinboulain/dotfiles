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
    endpoint = "[2603:c027:c000:5500:5f83:439:abd4:89aa]:51820";
    publicKey = "oUF5uaPNobTxDeYs4Q8nvsa8aJUFpYH397Lu6kfR2Ew=";
  };
}
