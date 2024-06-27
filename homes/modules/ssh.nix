{ pkgs, ... }:
{
  programs.ssh = {
    # I'm not really comfortable with Home Manager's way of doing things
    # (dumping a lot of defaults in the configuration instead of relying on the
    # actual defaults of the software) but in this case I'm gonna go with it
    # (after having verified the settings truly reflect the default values).
    enable = true;
    includes = [ "~/.ssh/config.local" ];
  };
}
