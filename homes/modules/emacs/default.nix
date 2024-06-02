{ lib, pkgs, ... }:
{
  programs.emacs = {
    enable = true;
    package = pkgs.emacs-nox;
    extraPackages =
      epkgs: with epkgs; [
        ace-window
        avy
        benchmark-init
        bqn-mode
        circe
        clang-format
        corfu
        corfu-terminal
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
        rust-mode
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
    # TODO: get rid of this once the fork is official:
    # https://github.com/emacs-rustic/rustic/issues/1
    overrides = final: parent: {
      rustic = parent.melpaPackages.rustic.overrideAttrs (old: {
        src = pkgs.fetchFromGitHub {
          owner = "emacs-rustic";
          repo = "rustic";
          rev = "d0f839244a5fdd9f4954034fa22d26e66d05115e";
          sha256 = "sha256-OfZNkuscbHKWvg+eEirt5Nhwb0Pm5Vwc0e+IcWA2tto=";
        };
      });
    };
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

  programs.bash.initExtra = lib.mkAfter ''
    if type -p emacs &> /dev/null; then
      export EDITOR='emacs -nw'
    fi
  '';
}
