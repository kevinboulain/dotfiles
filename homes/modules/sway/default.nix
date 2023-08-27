{ config, lib, pkgs, swaybar, ... }:
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

      xdg.configFile = {
        foot = {
          source = ./foot;
          recursive = true;
        };
        gammastep = {
          source = ./gammastep;
          recursive = true;
        };
        mako = {
          source = ./mako;
          recursive = true;
        };
        sway = {
          source = ./sway;
          recursive = true;
        };
        "sway/config.d/theme".text = ''
          # (insert (my-export-theme-sway))
          default_border pixel 1
          smart_gaps on
          gaps inner 3
          output * bg #171717 solid_color
          client.focused #00000000 #616161 #00000000 #9E9E9E
          client.focused_inactive #00000000 #252525 #00000000 #3C3C3C
          client.unfocused #00000000 #252525 #00000000 #3C3C3C

          bar {
            mode dock
            position bottom
            font "Iosevka Term 8"
            status_command ${swaybar.packages.${pkgs.system}.default}/bin/swaybar 2>> "''${XDG_RUNTIME_DIR:-/tmp}"/swaybar.log
            colors {
              background #171717
              statusline #DADADA
              separator #616161
              focused_workspace #171717 #171717 #FAFAFA
              active_workspace #171717 #171717 #DADADA
              inactive_workspace #171717 #171717 #616161
            }
          }
        '';
      };

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
