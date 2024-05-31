{ pkgs, ... }:
{
  # I don't think I'll opt into Home Manager's tmux module: it's too opinionated
  # and I would have to undo things (e.g.: the terminal is still set to screen
  # though the terminfo database has long been updated), like for Bash...
  home.packages = with pkgs; [ tmux ];
  xdg.configFile.tmux = {
    source = ./tmux;
    recursive = true;
  };
}
