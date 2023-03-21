{ myLib, pkgs, ... }: {
  programs.emacs = {
    enable = true;
    package = pkgs.emacs-nox;
    extraPackages = epkgs: with epkgs; [
      ace-window
      avy
      benchmark-init
      bqn-mode
      circe
      cmake-mode
      company
      dart-mode
      eglot
      ethan-wspace
      flycheck
      geiser
      geiser-guile
      go-mode
      htmlize
      idris-mode
      interaction-log
      lua-mode
      magit
      marginalia
      markdown-mode
      move-text
      nix-mode
      orderless
      poly-markdown
      poly-org
      poly-rst
      rainbow-delimiters
      rustic
      s
      scad-mode
      tao-theme
      toc-org
      use-package
      vertico
      which-key
      yasnippet
    ];
  };
  # https://www.gnu.org/software/emacs/manual/html_node/emacs/Find-Init.html
  home.file = (myLib.copyTrees ./. [ ".emacs.d" ]) // {
    ".emacs.d/readme.org".source = ./readme.org;
  };
  home.packages = with pkgs; [
    hunspell
    hunspellDicts.en-us
    hunspellDicts.fr-any
  ];
}
