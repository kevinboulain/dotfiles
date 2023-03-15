{ pkgs, ... }:
{
  # I don't really care about a fancy cursor but that's apparently required:
  # https://kevin.stravers.net/WaylandCursor
  # https://nixos.org/manual/nixos/stable/index.html#sec-gnome-icons-and-gtk-themes
  environment.systemPackages = with pkgs; [
    gnome.adwaita-icon-theme
  ];

  environment.etc = {
    "sway/config.d/cursor".text = ''
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
  };
}
