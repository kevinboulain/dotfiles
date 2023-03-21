{ pkgs, ... }: {
  # To set up OpenPGP: https://github.com/drduh/YubiKey-Guide
  # To set up OpenSSH:
  #  ssh-keygen -t ed25519-sk -O resident -O application=ssh:git@github.com -f ~/.ssh/github
  #  ssh-keygen -K
  # Note the 'private' key isn't private, only a handle.
  home.packages = with pkgs; [
    gnupg
    (pass.withExtensions (extensions: with extensions; [ pass-otp ]))
    pinentry-curses
    yubikey-manager
  ];
  services.gpg-agent = {
    enable = true;
    pinentryFlavor = "curses";
    enableSshSupport = true;
  };
}
