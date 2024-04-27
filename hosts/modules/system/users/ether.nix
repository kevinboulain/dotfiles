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
  users.users.ether = {
    isNormalUser = true;
    hashedPasswordFile = config.sops.secrets.ether.path;
    openssh.authorizedKeys.keys = [ myPublicKey ];
    home = "${userHomeDirectory}/ether";
    extraGroups = [ "wheel" ];
  };

  services.openssh.extraConfig = lib.mkAfter ''
    AllowUsers ether
  '';
}
