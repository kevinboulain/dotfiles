{ config, mySystemDirectory, ... }: {
  # etcdctl --cacert /nix/store/...-ca.pem --cert /nix/store/...-kubernetes-01.pem --key /run/secrets/etcdClientKey --endpoints kubernetes-01:2379 member list
  services.etcd = {
    enable = true;

    # TODO: understand these settings better
    advertiseClientUrls = [ "https://${config.networking.fqdnOrHostName}:2379" ];
    initialAdvertisePeerUrls = [ "https://${config.networking.fqdnOrHostName}:2380" ];
    initialCluster = map (peer: "${peer.fqdnOrHostName}=https://${peer.fqdnOrHostName}:2380") config.my.mesh.peers;
    # TODO: brackets should be derived automatically
    # 'expected IP in URL for binding (https://kubernetes-01:2380)'
    listenClientUrls = [
      "https://[${config.my.mesh.address}]:2379"
      # "https://[::1]:2379" "https://127.0.0.1:2379"
    ];
    listenPeerUrls = [
      "https://[${config.my.mesh.address}]:2380"
      # "https://[::1]:2380" "https://127.0.0.1:2380"
    ];

    peerClientCertAuth = true;
    peerTrustedCaFile = ./certificates/etcd/peer/ca.pem;
    peerKeyFile = config.sops.secrets.etcdPeerKey.path;
    peerCertFile = ./certificates/etcd/peer/. + "/${config.networking.fqdnOrHostName}.pem";

    clientCertAuth = true;
    trustedCaFile = ./certificates/etcd/client/ca.pem;
    keyFile = config.sops.secrets.etcdClientKey.path;
    certFile = ./certificates/etcd/client/server/. + "/${config.networking.fqdnOrHostName}.pem";

    dataDir = "${mySystemDirectory}/var/lib/etcd";
  };

  networking.firewall.interfaces.${config.my.mesh.interface}.allowedTCPPorts = [ 2379 2380 ];
}
