{ nixpkgs, ... }:
let
  # I don't think there's an official place for that but /etc/nix/registry.json
  # also references the same file so the directory seems fitting.
  nixpkgsPath = "nix/channels/nixpkgs";
in
{
  # Replace system channels with the flake's input, so that everything points to
  # the same version.
  # https://discourse.nixos.org/t/do-flakes-also-set-the-system-channel/19798
  nix = {
    settings.experimental-features = ["nix-command" "flakes"];
    registry.nixpkgs.flake = nixpkgs;
    nixPath = [ "nixpkgs=/etc/${nixpkgsPath}" ];
  };
  environment.etc."${nixpkgsPath}".source = nixpkgs;
}
