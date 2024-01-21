{ config, lib, myStateDirectory, ... }:
let
  inherit (import ../modules/system/users/lib.nix { inherit myStateDirectory; }) sopsUserPassword;
in
{
  imports = [
    ./networking.nix
    ./system.nix
    ./etcd.nix
    ./api-server.nix # TODO: kuba-apiserver vs api-server
  ];

  sops.secrets =
    let
      default = ./. + "/${lib.strings.removePrefix "kubernetes-" config.networking.fqdnOrHostName}/default.yaml";
    in
      {
        root = sopsUserPassword default;
        meshPrivateKey.sopsFile = default;
        # TODO: move that to etcd.nix
        etcdPeerKey = {
          sopsFile = default;
          owner = config.systemd.services.etcd.serviceConfig.User;
        };
        etcdClientKey = {
          sopsFile = default;
          owner = config.systemd.services.etcd.serviceConfig.User;
        };

        apiServerEtcdKey = { # Rename to apiserveretcdclientkey?
          sopsFile = default;
          # owner = config.systemd.services.kube-apiserver.serviceConfig.User;
        };
      };
}
