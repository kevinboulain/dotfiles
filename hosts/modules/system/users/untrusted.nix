{ config, lib, myStateDirectory, ... }:
let
  inherit (import ./lib.nix { inherit myStateDirectory; }) userHomeDirectory;
in
{
  users.users.untrusted = {
    isNormalUser = true;
    passwordFile = config.sops.secrets.untrusted.path;
    home = "${userHomeDirectory}/untrusted";
    homeMode = "0750";
  };

  services.openssh.extraConfig = lib.mkAfter ''
    # Should be safe. Resolving localhost would require UseDNS.
    AllowUsers untrusted@127.0.0.1 untrusted@::1
  '';

  services.logind.extraConfig = ''
    KillOnlyUsers=untrusted
  '';
}
