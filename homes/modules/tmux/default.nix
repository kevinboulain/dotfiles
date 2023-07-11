{ pkgs, ... }: {
  home.packages = with pkgs; [
    # I'm not sure I want to opt into Home Manager's defaults...
    tmux
  ];
  xdg.configFile.tmux = {
    source = ./tmux;
    recursive = true;
  };
}
