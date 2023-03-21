{ pkgs, ... }: {
  services.udev.packages = with pkgs; [
    # Make the key accessible to non-root users with udev rules.
    yubikey-personalization
  ];
  services.pcscd.enable = true;
}
