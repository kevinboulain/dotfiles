{ ... }: {
  # https://nix-community.github.io/home-manager/index.html#sec-flakes-nixos-module
  home-manager = {
    useGlobalPkgs = true;
    useUserPackages = true;
  };
}
