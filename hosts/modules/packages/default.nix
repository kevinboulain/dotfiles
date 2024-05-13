{ lib, ... }:
{
  # Get rid of any package installed by default (like nano).
  environment.defaultPackages = [ ];
  programs.nano.enable = false; # Nano has been removed from environment.defaultPackages...
  # And of documentation.
  documentation.enable = lib.mkDefault false;
}
