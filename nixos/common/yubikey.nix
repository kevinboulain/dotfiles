{ pkgs, ... }:
{
  # To set up OpenPGP: https://github.com/drduh/YubiKey-Guide
  # To set up OpenSSH:
  #  ssh-keygen -t ed25519-sk -O resident -O application=ssh:git@github.com -f ~/.ssh/github
  #  ssh-keygen -K
  # Note the 'private' key isn't private, only a handle.
  services.udev.packages = with pkgs; [
    # Make the key accessible to non-root users with udev rules.
    yubikey-personalization
  ];
  services.pcscd.enable = true;
  environment.systemPackages = with pkgs; [
    (pass.withExtensions (extensions: with extensions; [ pass-otp ]))
    pinentry-curses
    yubikey-manager
  ];
  programs.gnupg.agent = {
    enable = true;
    pinentryFlavor = "curses";
  };
}
