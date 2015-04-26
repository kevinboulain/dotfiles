; haskell mode setup

(defvar haskell-mode "~/.emacs.d/haskell-mode/")

; test if the submodule exists
(when (file-readable-p haskell-mode)
  ; add it to load path
  (add-to-list 'load-path haskell-mode)

  ; test if can load haskell-mode-autoloads.el
  ; use make or make haskell-mode-autoloads.el
  (when (require 'haskell-mode-autoloads nil t)
    ; available modes
    (add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)
    ; (add-hook 'haskell-mode-hook 'turn-on-haskell-simple-indent)
    ; (add-hook 'haskell-mode-hook 'turn-on-haskell-indent)

    ; simple setup of hoogle (open a browser)
    (global-set-key (kbd "\C-ch") 'haskell-hoogle)
    (setq haskell-hoogle-command nil)
    ; could use hoogle command, if available
    ; (setq haskell-hoogle-command "hoogle")

    ; haskell lint setup
    (when (require 'hs-lint nil t)
      (defun hs-lint-call () (local-set-key "\C-cl" 'hs-lint))
      (add-hook 'haskell-mode-hook 'hs-lint-call)
      ; move the cursor everytime, a bit too annoying for a save hook
      ; (add-hook 'after-save-hook 'hs-lint)
    )

    ; setup ghc-mod
    (when (require 'ghc nil t)
      (ghc-init)
      (ghc-debug)
      (add-hook 'haskell-mode-hook (lambda () (ghc-init)))
    )
  )
)
