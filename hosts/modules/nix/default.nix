{ pkgs, ... }: {
  imports = [ ./flakes.nix ];

  environment.variables = {
    NIX_SHELL_PRESERVE_PROMPT = "1";
  };

  environment.systemPackages = with pkgs; [
    cntr  # https://nixos.org/manual/nixpkgs/stable/#breakpointhook
    nix-diff  # To compare derivations, e.g.: nix-diff /nix/var/nix/profiles/system-{1,2}-link
    nix-index  # To search packages.
    nix-tree  # Convenient dependency graphs.
    nixos-option  # To evaluate the configuration.
    nvd  # To compare system images, e.g.: nvd diff /nix/var/nix/profiles/system-{1,2}-link
  ];

  nix.settings.allowed-users = [
    # Prevent anyone not in the wheel group from connecting to the Nix daemon.
    # See also trusted-users, which allows root by default.
    "@wheel"
    # But also include regular users: for better or worse, this makes it less
    # annoying for the untrusted user and is required by Home Manager.
    "@users"
  ];
}
