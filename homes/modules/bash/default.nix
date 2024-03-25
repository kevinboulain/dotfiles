{ config, lib, pkgs, ... }: {
  programs.bash.enable = true;
  # Override Home Manager's (including history setup, shopts and aliases that I
  # don't want to manage here nor have defaults forced on me).
  home.file.".bashrc".source = lib.mkForce (pkgs.writeText "bashrc" ''
    # Home Manager's stuff.
    ${config.programs.bash.bashrcExtra}

    # Don't do anything when invoked as a non-interactive shell.
    [[ $- == *i* ]] || return
    # The whole configuration is documented in the readme.org file.
    . <(sed '/^#+begin_src shell$/,/^#+end_src$/!d;//d' ${./readme.org})

    # Home Manager's stuff (e.g.: gpg-agent's GPG_TTY).
    ${config.programs.bash.initExtra}
  '');
  programs.bash.initExtra = lib.mkAfter ''
    if type -p less &> /dev/null; then
      export PAGER='less -r'
    fi
  '';
}
