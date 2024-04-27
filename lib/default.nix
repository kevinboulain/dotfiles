{ lib, ... }: {
  allowUnfreePredicate = pkgs: pkg: builtins.elem (lib.getName pkg) pkgs;
}
