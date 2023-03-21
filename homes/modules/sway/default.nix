{ config, lib, myLib, pkgs, ... }:
with lib;
let
  cfg = config.wayland.windowManager.sway;
in
{
  options.wayland.windowManager.sway = {
    hiDPIFix = mkOption {
      type = types.bool;
      default = false;
    };
  };

  config = mkMerge [
    {
      programs.foot = {
        enable = true;
        settings.main.include = "~/.config/foot/theme.ini";
      };

      xdg.configFile = myLib.copyTrees ./. [
        "foot"
        "gammastep"
        "mako"
        "sway"
        "xkb"
      ];

      home.packages = with pkgs; [
        fzf
        gammastep
        mako
        swayidle
      ];
    }
    (mkIf cfg.hiDPIFix {
      # I don't really care about a fancy cursor but that's apparently required:
      # https://kevin.stravers.net/WaylandCursor
      # https://nixos.org/manual/nixos/stable/index.html#sec-gnome-icons-and-gtk-themes
      home.packages = with pkgs; [
        gnome.adwaita-icon-theme
      ];

      xdg.configFile."sway/config.d/cursor".text = ''
        set $cursor_theme Adwaita
        # The cursor theme has to support the size or the cursor will change when
        # hovering X windows.
        set $cursor_size 22
        seat seat0 xcursor_theme $cursor_theme $cursor_size
        exec_always {
          dconf write /org/gnome/desktop/interface/cursor-theme "'$cursor_theme'"
          dconf write /org/gnome/desktop/interface/cursor-size $cursor_size
        }
      '';
    })
  ];
}
