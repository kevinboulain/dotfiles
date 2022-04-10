;; guix home --dry-run --load-path="$(pwd)" reconfigure my/home/minimal.scm

(define-module (my home minimal)
  #:use-module (gnu home)
  #:use-module (gnu packages aspell)
  #:use-module (gnu packages curl)
  #:use-module (gnu packages emacs)
  #:use-module (gnu packages libreoffice)
  #:use-module (gnu packages package-management)
  #:use-module (gnu packages tmux)
  #:use-module (gnu packages version-control)
  #:export (%minimal-packages))

;; @code{home-environment-packages} isn't exported (otherwise we could inherit
;; from the home environment below?).
(define %minimal-packages
  (list
   ;; Utilities.
   curl
   git
   stow ; To install the dotfiles.
   tmux
   ;; Emacs.
   emacs-no-x
   hunspell
   hunspell-dict-en-us
   hunspell-dict-fr-toutes-variantes))

(home-environment
 (packages %minimal-packages))
