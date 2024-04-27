{ pkgs, ... }:
{
  environment.systemPackages = with pkgs; [
    cntr # https://nixos.org/manual/nixpkgs/stable/#breakpointhook
    nix-diff # To compare derivations, e.g.: nix-diff /nix/var/nix/profiles/system-{1,2}-link
    nix-index # To search packages.
    nix-tree # Convenient dependency graphs.
    nixos-option # To evaluate the configuration.
    nvd # To compare system images, e.g.: nvd diff /nix/var/nix/profiles/system-{1,2}-link
  ];
}
