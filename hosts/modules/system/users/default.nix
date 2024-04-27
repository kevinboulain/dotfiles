{
  config,
  lib,
  myPublicKey,
  myStateDirectory,
  ...
}:
let
  inherit (import ./lib.nix { inherit myStateDirectory; }) userHomeDirectory;
in
{
  systemd.tmpfiles.rules = [ "d ${userHomeDirectory} 750 root users - -" ];

  users = {
    mutableUsers = false;
    users.root = {
      hashedPasswordFile = config.sops.secrets.root.path;
      openssh.authorizedKeys.keys = [ myPublicKey ];
    };
  };

  services.openssh.extraConfig = lib.mkAfter ''
    AllowUsers root
  '';
}
