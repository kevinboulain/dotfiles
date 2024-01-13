{ pkgs, ... }: {
  imports = [
    ./flakes.nix
    ./unfree.nix
  ];

  environment.variables = {
    NIX_SHELL_PRESERVE_PROMPT = "1";
  };

  nix.settings.allowed-users = [
    # Prevent anyone not in the wheel group from connecting to the Nix daemon.
    # See also trusted-users, which allows root by default.
    "@wheel"
    # But also include regular users: for better or worse, this makes it less
    # annoying for the untrusted user and is required by Home Manager.
    "@users"
  ];
}
