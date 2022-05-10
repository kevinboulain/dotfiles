{ ... }:
{
  i18n.defaultLocale = "en_US.UTF-8";
  console.useXkbConfig = true; # use xkbOptions in tty.
  services.xserver = {
    layout = "us";
    xkbVariant = "intl";
    xkbOptions = "caps:ctrl_modifier";  # TODO
  };
}
