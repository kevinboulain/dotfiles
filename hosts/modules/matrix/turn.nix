# https://nixos.wiki/wiki/Matrix#Coturn_with_Synapse
{ config, lib, ... }:
let
  inherit (import ../nginx/lib.nix { inherit lib; }) mergeVirtualHostFragments virtualHostFragments;
in
{
  services.nginx.virtualHosts.turn = mergeVirtualHostFragments [
    virtualHostFragments.disallowRobots
    virtualHostFragments.emptyCatchAll
    virtualHostFragments.explicitServerName
    # Stolen from the nginx module. The module doesn't seem to export a way to
    # simply serve the challenge. It's also made harder by the fact that
    # config.security.acme.defaults.webroot isn't set (but the vhost's acmeRoot
    # is).
    {
      locations."~ ^/.well-known/acme-challenge/".extraConfig = ''
        root "${config.services.nginx.virtualHosts.turn.acmeRoot}";
        try_files $uri =404;
      '';
    }
  ];
  systemd.services.nginx.before = [ "acme-${config.services.coturn.realm}.service" ];
  security.acme.certs.${config.services.coturn.realm} = {
    group = config.systemd.services.coturn.serviceConfig.Group;
    webroot = config.services.nginx.virtualHosts.turn.acmeRoot;
    # The name property doesn't appear to be available though it's documented.
    reloadServices = [
      (
        assert config.systemd.services.coturn.enable;
        "coturn"
      )
    ];
  };

  sops.secrets.matrixTurnSharedSecret = {
    key = "turn_shared_secret";
    group = config.systemd.services.coturn.serviceConfig.Group;
    mode = "0440";
  };

  # https://matrix-org.github.io/synapse/latest/setup/turn/coturn.html
  services.coturn = rec {
    enable = true;
    use-auth-secret = true;
    static-auth-secret-file = config.sops.secrets.matrixTurnSharedSecret.path;
    realm = config.services.nginx.virtualHosts.turn.serverName;
    cert = "${config.security.acme.certs.${realm}.directory}/full.pem";
    pkey = "${config.security.acme.certs.${realm}.directory}/key.pem";
    no-cli = true; # No remote CLI.
    no-tcp-relay = true; # VoIP is UDP.
    min-port = 50000;
    max-port = 51000;
    extraConfig = ''
      prometheus

      # See the recommended setup.
      denied-peer-ip=10.0.0.0-10.255.255.255
      denied-peer-ip=192.168.0.0-192.168.255.255
      denied-peer-ip=172.16.0.0-172.31.255.255
      no-multicast-peers
      denied-peer-ip=0.0.0.0-0.255.255.255
      denied-peer-ip=100.64.0.0-100.127.255.255
      denied-peer-ip=127.0.0.0-127.255.255.255
      denied-peer-ip=169.254.0.0-169.254.255.255
      denied-peer-ip=192.0.0.0-192.0.0.255
      denied-peer-ip=192.0.2.0-192.0.2.255
      denied-peer-ip=192.88.99.0-192.88.99.255
      denied-peer-ip=198.18.0.0-198.19.255.255
      denied-peer-ip=198.51.100.0-198.51.100.255
      denied-peer-ip=203.0.113.0-203.0.113.255
      denied-peer-ip=240.0.0.0-255.255.255.255
    '';
  };

  services.prometheus.scrapeConfigs = [
    {
      job_name = "coturn";
      static_configs = [ { targets = [ "localhost:9641" ]; } ];
    }
  ];

  networking.firewall = with config.services.coturn; {
    # See the man page: plain and TLS clients can actually connect to any
    # listening socket and the server will handle it transparently. I'm not
    # familiar with TURN but it's apparently not really required to enable TLS
    # (though, it may help with privacy):
    # https://github.com/coturn/coturn/issues/33
    allowedTCPPorts = [
      listening-port
      tls-listening-port
    ];
    allowedUDPPorts = [
      listening-port
      tls-listening-port
    ];
    allowedUDPPortRanges = [
      {
        from = min-port;
        to = max-port;
      }
    ];
  };

  # https://matrix-org.github.io/synapse/latest/setup/turn/coturn.html
  # services.matrix-synapse.turn_shared_secret only takes in the secret as a
  # string and there's no turn_shared_secret_path like
  # registration_shared_secret_path...
  sops.templates."matrix_turn_shared_secret.yaml" = {
    mode = "0440";
    group = config.systemd.services.matrix-synapse.serviceConfig.Group;
    content = builtins.toJSON { turn_shared_secret = config.sops.placeholder.matrixTurnSharedSecret; };
  };
  services.matrix-synapse = with config.services.coturn; {
    # No 'turn:' because it may help with privacy and restrictive environments.
    # I don't appear to be facing this issue but be aware of
    # https://github.com/element-hq/element-android/issues/1533
    settings.turn_uris = [
      "turns:${realm}?transport=tcp"
      "turns:${realm}?transport=udp"
    ];
    extraConfigFiles = [ config.sops.templates."matrix_turn_shared_secret.yaml".path ];
  };
}
