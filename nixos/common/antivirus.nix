{ ... }:
{
  services.clamav = {
    daemon.enable = true;
    # TODO: I'm not sure I want to run the updater automatically.
    # See also https://github.com/NixOS/nixpkgs/issues/187992
    # updater.enable = true;
  };
}
