(defconst rust-mode (concat user-emacs-directory "rust"))

(when (file-readable-p rust-mode)
  (add-to-list 'load-path rust-mode)

  (when (require 'rust-mode nil t)
    (add-to-list 'auto-mode-alist '("\\.rs$" . rust-mode))
    (setq rust-indent-offset tab-width)
  )
)

; flycheck rust mode (cargo project handling)
(defconst flycheck-rust (concat user-emacs-directory "flycheck-rust"))

(when (file-readable-p flycheck-rust)
  (add-to-list 'load-path flycheck-rust)

  (if (featurep 'flycheck)
    (when (require 'flycheck-rust nil t)
      (add-hook 'flycheck-mode-hook 'flycheck-rust-setup) ; add a flycheck hook
    )
    (message "Could not load flycheck-rust: missing dependencies")
  )
)

; racer mode (rust code completion)
(defconst racer (concat user-emacs-directory "racer"))

(when (file-readable-p racer)
  (add-to-list 'load-path racer)

  ; requirements:
  ;   company
  ;   dash
  ;   f
  ;   s
  ;   rust
  (if (and (featurep 'company)
           (featurep 'dash)
           (featurep 'f)
           (featurep 's)
           (featurep 'rust-mode))
    (when (require 'racer nil t)
      (add-hook 'rust-mode-hook 'racer-mode)
      ; if $PATH and $RUST_SRC_PATH are not set, use the following
      ; (setq racer-cmd "<path-to-racer-srcdir>/target/release/racer")
      ; (setq racer-rust-src-path "<path-to-rust-srcdir>/src/")
    )
    (message "Could not load racer: missing dependencies")
  )
)
