{ pkgs, ... }: {
  # To set up OpenPGP: https://github.com/drduh/YubiKey-Guide
  # To set up OpenSSH:
  #  ssh-keygen -t ed25519-sk -O resident -O verify-required -O application=ssh:git@github.com -f ~/.ssh/github.com
  #  ssh-keygen -K
  # Note the 'private' key isn't private, only a handle. And without
  # 'verify-required', the PIN would not be asked.
  home.packages = with pkgs; [
    gnupg
    (pass.withExtensions (extensions: with extensions; [ pass-otp ]))
    pinentry-curses
    yubikey-manager
  ];
  services.gpg-agent = {
    enable = true;
    # Add the following to .ssh/config to work around unavoidable issues:
    #  Match host * exec "gpg-connect-agent updatestartuptty /bye"
    pinentryFlavor = "curses";
    enableSshSupport = true;
  };
}
