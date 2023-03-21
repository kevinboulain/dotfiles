{ ... }: {
  i18n.defaultLocale = "en_US.UTF-8";
  console.useXkbConfig = true;  # Use xkbOptions in tty.
  services.xserver = {
    layout = "us";
    xkbVariant = "intl";
    xkbOptions = "ctrl:nocaps";
  };
}
