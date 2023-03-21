{ config, lib, pkgs, ... }: {
  programs.bash.enable = true;
  # Override Home Manager's.
  home.file.".bashrc".source = lib.mkForce (pkgs.writeText "bashrc" ''
    # Don't do anything when invoked as a non-interactive shell.
    [[ $- == *i* ]] || return
    # The whole configuration is documented in the readme.org file.
    . <(sed '/^#+begin_src shell$/,/^#+end_src$/!d;//d' ${./readme.org})
    # Don't forget about Home Manager's stuff: this is important for some
    # integrations, like gpg-agent's GPG_TTY.
    ${config.programs.bash.initExtra}
  '');
}
