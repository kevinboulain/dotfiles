{ config, lib, pkgs, ... }:
with lib;
let
  inherit (config.my) mesh;
  port = endpoint: strings.toInt (lists.last (strings.splitString ":" endpoint));
  publicKeyToInterfaceIdentifier = publicKey:
    let
      hex = builtins.substring 0 16 (builtins.hashString "sha256" publicKey);
    in
      strings.concatStringsSep ":" [
        (builtins.substring 0 4 hex)
        (builtins.substring 4 4 hex)
        (builtins.substring 8 4 hex)
        (builtins.substring 12 4 hex)
      ];
  # TOOD: conflict with qemu subnet
  publicKeyToIP = publicKey: "fd2a::" + (publicKeyToInterfaceIdentifier publicKey);
  publicKeyToIPCIDR = publicKey: (publicKeyToIP publicKey) + "/64";
  publicOptions = {
    address = mkOption {
      type = types.nonEmptyStr;
      default = publicKeyToIP config.my.mesh.publicKey;
    };
    endpoint = mkOption {
      type = types.nonEmptyStr;
    };
    publicKey = mkOption {
      type = types.nonEmptyStr;
    };
    privateKeyFile = mkOption {
      type = types.path;
    };
    peers = mkOption {
      type = types.listOf types.unspecified;
      default = [];
    };
    interface = mkOption {
      type = types.nonEmptyStr;
      default = "mesh0";
    };
  };
in
{
  options.my.mesh = publicOptions // {
    peers = mkOption {
      type = types.listOf (types.submodule {
        options = publicOptions // {
          fqdnOrHostName = mkOption {
            type = types.nonEmptyStr;
          };
        };
      });
      default = [];
    };
  };

  config = mkIf (mesh.peers != []) {
    environment.systemPackages = [ pkgs.wireguard-tools ];

    systemd.network = {
      netdevs."99-${mesh.interface}" = {
        netdevConfig = {
          Kind = "wireguard";
          Name = mesh.interface;
          # TODO
          # MTUBytes = "1300";
        };
        wireguardConfig = {
          PrivateKeyFile = mesh.privateKeyFile;
          ListenPort = port mesh.endpoint;
        };
        wireguardPeers = map (peer: {
          wireguardPeerConfig = {
            Endpoint = peer.endpoint;
            PublicKey = peer.publicKey;
            AllowedIPs = [ peer.address ];
          };
        }) (filter (peer: peer.publicKey != mesh.publicKey) mesh.peers);
      };
      networks.${mesh.interface} = {
        matchConfig.Name = mesh.interface;
        address = [(publicKeyToIPCIDR mesh.publicKey)];
        DHCP = "no";
        networkConfig.IPv6AcceptRA = false;
      };
    };

    networking.firewall.allowedUDPPorts = [ (port mesh.endpoint) ];

    networking.hosts =builtins.listToAttrs (map (peer: {
      name = peer.address;
      value = [ peer.fqdnOrHostName ];
    }) (filter (peer: peer.publicKey != mesh.publicKey) mesh.peers));
  };
}
