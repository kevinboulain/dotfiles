{ pkgs, sin, ... }:
{
  home.packages = with pkgs; [
    notmuch
    notmuch.emacs
    sin.packages.${pkgs.system}.default
    slrn
  ];
}
