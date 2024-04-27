{ lib, ... }:
{
  # Get rid of any package installed by default (like nano).
  environment.defaultPackages = [ ];
  # And of documentation.
  documentation.enable = lib.mkDefault false;
}
