{ ... }:
{
  gtk = {
    enable = true;
    gtk3.extraConfig = {
      # https://wiki.archlinux.org/title/GTK
      gtk-application-prefer-dark-theme = true;
      # Overridden with gtk.css, it appears to be the only way to unbind the
      # annoying C-w in Firefox.
      # dconf write /org/gnome/desktop/interface/gtk-key-theme "'Emacs'"
      # gtk-key-theme-name = Emacs
    };
  };
  xdg.configFile."gtk-3.0" = {
    source = ./gtk-3.0;
    recursive = true;
  };
}
