; load haskell lint from cabal
(haskell-lint
   (concat
    (glob-last (glob-last "~/.cabal/share/" "x86_64-osx-ghc-.*") "hlint-*")
   "/hs-lint.el")
)
; load haskell ghc-mode from cabal
(haskell-ghc-mode
  (concat
    (glob-last (glob-last "~/.cabal/share/" "x86_64-osx-ghc-.*") "ghc-mod-*")
   "/ghc.el")
)

; (turn-on-pbcopy) done via terminal-init-xterm-hook, from the modules directory
(require 'pbcopy)

; enable gud (lldb debugger in emacs mode), works out of the box on Mac OS X
(require 'gud)

; mouse support, for now not really useful?
(unless window-system
  ; mouse module
  (require 'mouse)
  (xterm-mouse-mode t)
  (global-set-key [mouse-4] (lambda () (interactive) (scroll-down 1)))
  (global-set-key [mouse-5] (lambda () (interactive) (scroll-up 1)))
  (defun track-mouse (e))
  (setq mouse-sel-mode t)
)
