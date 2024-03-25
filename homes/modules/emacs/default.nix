{ pkgs, ... }: {
  programs.emacs = {
    enable = true;
    package = pkgs.emacs-nox;
    extraPackages = epkgs: with epkgs; [
      ace-window
      avy
      benchmark-init
      bqn-mode
      circe
      clang-format
      company
      eglot
      ethan-wspace
      flycheck
      glsl-mode
      htmlize
      interaction-log
      jinx
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
      tao-theme
      toc-org
      treesit-grammars.with-all-grammars
      use-package
      vertico
      which-key
      yasnippet
    ];
  };
  # https://www.gnu.org/software/emacs/manual/html_node/emacs/Find-Init.html
  home.file = {
    ".emacs.d" = {
      source = ./.emacs.d;
      recursive = true;
    };
    ".emacs.d/readme.org".source = ./readme.org;
  };
  home.packages = with pkgs; [
    hunspellDicts.en-us
    hunspellDicts.fr-any
  ];
}
