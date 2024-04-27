{ ... }:
{
  i18n.defaultLocale = "en_US.UTF-8";
  console.useXkbConfig = true; # Use xkb.options in tty.
  services.xserver.xkb = {
    layout = "us";
    variant = "intl";
    options = "ctrl:nocaps";
  };
}
