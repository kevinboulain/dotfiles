{ pkgs, ... }: {
  imports = [
    ./minimal.nix
    ./modules/firefox.nix
    ./modules/gtk
    ./modules/sway
  ];

  home.packages = with pkgs; [
    # General desktop utilities.
    imv
    mpv
    pavucontrol
    zathura

    # Screenshots & co.
    grim
    slurp
    wl-clipboard

    # For Emacs.
    notmuch
    notmuch.emacs
    slrn

    # More fonts.
    (iosevka-bin.override { variant = "aile"; })
    (iosevka-bin.override { variant = "etoile"; })
  ];

  # Handle media key on bluetooth headsets.
  # https://wiki.archlinux.org/title/MPRIS#Bluetooth
  services.mpris-proxy.enable = true;
}
