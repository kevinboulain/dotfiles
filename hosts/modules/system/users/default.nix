{ config, myPublicKey, myStateDirectory, ... }:
let
  inherit (import ./lib.nix { inherit myStateDirectory; }) sopsUserPassword userHomeDirectory;
in
{
  systemd.tmpfiles.rules = [ "d ${userHomeDirectory} 750 root users - -" ];

  sops.secrets = {
    ether = sopsUserPassword;
    root = sopsUserPassword;
  };

  users = {
    mutableUsers = false;
    users = {
      ether = {
        isNormalUser = true;
        passwordFile = config.sops.secrets.ether.path;
        openssh.authorizedKeys.keys = [ myPublicKey ];
        home = "${userHomeDirectory}/ether";
        extraGroups = [ "wheel" ];
      };
      root = {
        passwordFile = config.sops.secrets.root.path;
        openssh.authorizedKeys.keys = [ myPublicKey ];
      };
    };
  };
}
