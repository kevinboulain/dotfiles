{ config, lib, pkgs, ... }:
let
  securePort = 6443;
in
{

  systemd.services.kube-apiserver = {
    after = [ "network.target" ];
    wantedBy = [ "kubernetes.target" ];
    serviceConfig = {
      Slice = "kubernetes.slice";
      ExecStart = lib.strings.concatStringsSep " " [
        "${pkgs.kubernetes}/bin/kube-apiserver"
        "--allow-privileged=false"
        # TODO: localhost?
        "--bind-address=${config.my.mesh.address}"  # See also --advertise-address

        "--etcd-servers=${lib.strings.concatStringsSep "," (map (peer: "${peer.fqdnOrHostName}=https://${peer.fqdnOrHostName}:2380") config.my.mesh.peers)}"
        "--etcd-cafile=${./certificates/etcd/client/ca.pem}"
        "--etcd-certfile=${./certificates/etcd/client/client/. + "/${config.networking.fqdnOrHostName}.pem"}"
        "--etcd-keyfile=${config.sops.secrets.apiServerEtcdKey.path}"

        # "--runtime-config=${cfg.runtimeConfig}"

        "--secure-port=${builtins.toString securePort}"

        # "--service-account-issuer=${toString cfg.serviceAccountIssuer}"
        # "--service-account-signing-key-file=${cfg.serviceAccountSigningKeyFile}"
        # "--service-account-key-file=${cfg.serviceAccountKeyFile}"
        "--service-cluster-ip-range=fd2c::/112" # TODO: double check

        "--v=3"  # TODO
      ];

        # --disable-admission-plugins=${concatStringsSep "," cfg.disableAdmissionPlugins}
        # --enable-admission-plugins=${concatStringsSep "," cfg.enableAdmissionPlugins}
        # ${optionalString (cfg.featureGates != [])
        #   "--feature-gates=${concatMapStringsSep "," (feature: "${feature}=true") cfg.featureGates}"}
        # ${optionalString (cfg.proxyClientCertFile != null)
        #   "--proxy-client-cert-file=${cfg.proxyClientCertFile}"}
        # ${optionalString (cfg.proxyClientKeyFile != null)
        #   "--proxy-client-key-file=${cfg.proxyClientKeyFile}"}
        # ${optionalString (cfg.tokenAuthFile != null)
        #   "--token-auth-file=${cfg.tokenAuthFile}"}
        # ${optionalString (cfg.tlsCertFile != null)
        #   "--tls-cert-file=${cfg.tlsCertFile}"}
        # ${optionalString (cfg.tlsKeyFile != null)
        #   "--tls-private-key-file=${cfg.tlsKeyFile}"}
        # ${cfg.extraOpts}
        # --storage-backend=${cfg.storageBackend}
        # --api-audiences=${toString cfg.apiAudiences}
        # ${optionalString (cfg.preferredAddressTypes != null)
        #   "--kubelet-preferred-address-types=${cfg.preferredAddressTypes}"}
        # --authorization-mode=${concatStringsSep "," cfg.authorizationMode}
        #   ${optionalString (elem "ABAC" cfg.authorizationMode)
        #     "--authorization-policy-file=${
        #       pkgs.writeText "kube-auth-policy.jsonl"
        #         (concatMapStringsSep "\n" (l: builtins.toJSON l) cfg.authorizationPolicy)
        #     }"
        #    }
        # ${optionalString (elem "Webhook" cfg.authorizationMode)
        #   "--authorization-webhook-config-file=${cfg.webhookConfig}"
        #  }
        # ${optionalString (cfg.clientCaFile != null)
        #   "--client-ca-file=${cfg.clientCaFile}"}
        # ${optionalString (cfg.kubeletClientCaFile != null)
        #   "--kubelet-certificate-authority=${cfg.kubeletClientCaFile}"}
        # ${optionalString (cfg.kubeletClientCertFile != null)
        #   "--kubelet-client-certificate=${cfg.kubeletClientCertFile}"}
        # ${optionalString (cfg.kubeletClientKeyFile != null)
        #   "--kubelet-client-key=${cfg.kubeletClientKeyFile}"}
      WorkingDirectory = "/var/lib/kubernetes"; # TODO
      User = "kubernetes";
      Group = "kubernetes";
      AmbientCapabilities = "cap_net_bind_service";
      # Restart = "on-failure";
      # RestartSec = 5;
    };
    # unitConfig = {
    #   StartLimitIntervalSec = 0;
    # };
  };

  # services.kubernetes.apiserver = {
  #   enable = true;
  #   serviceClusterIpRange = "10.32.0.0/24";

  #   # Using ABAC for CoreDNS running outside of k8s
  #   # is more simple in this case than using kube-addon-manager
  #   authorizationMode = [ "RBAC" "Node" "ABAC" ];
  #   authorizationPolicy = corednsPolicies;

  #   etcd = {
  #     servers = etcdServers;
  #     caFile = "/var/lib/secrets/kubernetes/apiserver/etcd-ca.pem";
  #     certFile = "/var/lib/secrets/kubernetes/apiserver/etcd-client.pem";
  #     keyFile = "/var/lib/secrets/kubernetes/apiserver/etcd-client-key.pem";
  #   };

  #   clientCaFile = "/var/lib/secrets/kubernetes/ca.pem";

  #   kubeletClientCaFile = "/var/lib/secrets/kubernetes/ca.pem";
  #   kubeletClientCertFile = "/var/lib/secrets/kubernetes/apiserver/kubelet-client.pem";
  #   kubeletClientKeyFile = "/var/lib/secrets/kubernetes/apiserver/kubelet-client-key.pem";

  #   # TODO: separate from server keys
  #   serviceAccountKeyFile = "/var/lib/secrets/kubernetes/apiserver/server.pem";
  #   serviceAccountSigningKeyFile = "/var/lib/secrets/kubernetes/apiserver/server-key.pem";

  #   tlsCertFile = "/var/lib/secrets/kubernetes/apiserver/server.pem";
  #   tlsKeyFile = "/var/lib/secrets/kubernetes/apiserver/server-key.pem";
  # };

  networking.firewall.interfaces.${config.my.mesh.interface}.allowedTCPPorts = [ securePort ];
}
