; haskell mode
; to generate: make haskell-mode-autoloads.el
(when (file-readable-p "~/.emacs.d/haskell/haskell-mode-autoloads.el")
  (add-to-list 'load-path "~/.emacs.d/haskell/")
  (require 'haskell-mode-autoloads)
  ; available modes
  ; (add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)
  ; (add-hook 'haskell-mode-hook 'turn-on-haskell-simple-indent)
  (add-hook 'haskell-mode-hook 'turn-on-haskell-indent)
)

; load haskell lint if available
(defun haskell-lint (hlint-el)
  (when (file-readable-p hlint-el)
    (defvar hlint-el-directory (dirname hlint-el))
    (add-to-list 'load-path hlint-el-directory)

    (require 'hs-lint)
    (defun hs-lint-call () (local-set-key "\C-cl" 'hs-lint))
    (add-hook 'haskell-mode-hook 'hs-lint-call)
    ; move the cursor everytime, a bit too annoying for a save hook
    ; (add-hook 'after-save-hook 'hs-lint)
    )
)

; load ghc-mod if available
(defun haskell-ghc-mode (ghc-mode-el)
  (when (file-readable-p ghc-mode-el)
    (defvar ghc-mode-directory (dirname ghc-mode-el))
    (add-to-list 'load-path ghc-mode-directory)

    (autoload 'ghc-init "ghc" nil t)
    (autoload 'ghc-debug "ghc" nil t)
    (add-hook 'haskell-mode-hook (lambda () (ghc-init)))
    )
)
