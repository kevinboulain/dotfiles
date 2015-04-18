; cabal install ghc-mod hlint hoogle
; cabal creates a directory tree under x86_64-$os-ghc-$module
; where:
;   $os may be 'osx' or 'linux'
;   $module is the module name and its version
; we can use pattern match to find the latest version of ghc and of the module
; implying we want the latest ghc and the latest module for this ghc...

; nil if the path doesn't exist
(defvar cabal-latest (glob-last "~/.cabal/share/" "^x86_64-.*-ghc-.*$"))

; haskell mode
; to generate: make haskell-mode-autoloads.el
(when (file-readable-p "~/.emacs.d/haskell/haskell-mode-autoloads.el")
  (add-to-list 'load-path "~/.emacs.d/haskell/")
  (require 'haskell-mode-autoloads)
  ; available modes
  (add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)
  ; (add-hook 'haskell-mode-hook 'turn-on-haskell-simple-indent)
  ; (add-hook 'haskell-mode-hook 'turn-on-haskell-indent)

  ; load hoogle if available
  (defun haskell-hoogle (databases)
    (global-set-key (kbd "\C-ch") 'haskell-hoogle)
    ; hoogle buffer mode, comments for opening hoogle results into a browser
    ; need a local hoogle and existing databases
    (if (file-readable-p databases)
      ; hoogle binary may exist (hoogle has been installed via cabal)
      ; and databases exist, then clause: use hoogle cli command
      (setq haskell-hoogle-command "hoogle")
      ; no database found, else clause: use browser
      (setq haskell-hoogle-command nil)
    )
  )
  (when cabal-latest
    (haskell-hoogle (concat (glob-last cabal-latest "hoogle-*") "/databases"))
  )
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
(when cabal-latest
  (haskell-lint (concat (glob-last cabal-latest "hlint-*") "/hs-lint.el"))
)

; load ghc-mod if available
(defun haskell-ghc-mod (ghc-mod-el)
  (when (file-readable-p ghc-mod-el)
    (defvar ghc-mod-directory (dirname ghc-mod-el))
    (add-to-list 'load-path ghc-mod-directory)

    (autoload 'ghc-init "ghc" nil t)
    (autoload 'ghc-debug "ghc" nil t)
    (add-hook 'haskell-mode-hook (lambda () (ghc-init)))
  )
)
(when cabal-latest
  (haskell-ghc-mod (concat (glob-last cabal-latest "ghc-mod-*") "/ghc.el"))
)
