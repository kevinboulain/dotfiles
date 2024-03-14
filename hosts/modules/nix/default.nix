{ ... }: {
  imports = [ ./unfree.nix ];

  environment.variables = {
    NIX_SHELL_PRESERVE_PROMPT = "1";
  };

  nix.settings.experimental-features = [ "nix-command" "flakes" ];
  # Before nixpkgs.flake.setNixPath (by default set to the flake's source), it
  # was possible to override the system channel manually:
  # https://discourse.nixos.org/t/do-flakes-also-set-the-system-channel/19798

  nix.settings.allowed-users = [
    # Prevent anyone not in the wheel group from connecting to the Nix daemon.
    # See also trusted-users, which allows root by default.
    "@wheel"
    # But also include regular users: for better or worse, this makes it less
    # annoying for the untrusted user and is required by Home Manager.
    "@users"
  ];
}
