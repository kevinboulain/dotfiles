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
    endpoint = "[2603:c027:c000:5500:6ed3:c962:d5c0:6c08]:51820";
    publicKey = "6bXgCFYUF7J/kA61ePWvVUojAb6VYTKU/TPaxzKm3TM=";
  };
}
