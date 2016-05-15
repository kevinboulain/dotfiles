; rust mode setup

(defconst rust-mode "~/.emacs.d/rust/")

; test if the submodule exists
(when (file-readable-p rust-mode)
  ; add it to load path
  (add-to-list 'load-path rust-mode)

  (when (require 'rust-mode nil t)
    (add-to-list 'auto-mode-alist '("\\.rs$" . rust-mode))
  )
)

; flycheck rust mode (cargo project handling)
(defconst flycheck-rust "~/.emacs.d/flycheck-rust/")

; test if the submodule exists
(when (file-readable-p flycheck-rust)
  ; add it to load path
  (add-to-list 'load-path flycheck-rust)

  (if (featurep 'flycheck)
    (when (require 'flycheck-rust nil t)
      ; add a flycheck hook
      (add-hook 'flycheck-mode-hook 'flycheck-rust-setup)
    )
    (message "Could not load flycheck-rust: missing dependencies")
  )
)

; racer mode (rust code completion)
(defconst racer "~/.emacs.d/racer/")

; test if the submodule exists
(when (file-readable-p racer)
  ; add it to load path
  (add-to-list 'load-path racer)

  ; activate module
  ; require:
  ;   s
  ;   dash
  ;   company
  (if (and (featurep 's)
           (featurep 'dash)
           (featurep 'company))
    (when (require 'racer nil t)
      ; if rust-mode is loaded, also load racer
      (when (featurep 'rust-mode)
        (add-hook 'rust-mode-hook 'racer-mode))
      ; if $PATH and $RUST_SRC_PATH are not set, use the following
      ; (setq racer-cmd "<path-to-racer-srcdir>/target/release/racer")
      ; (setq racer-rust-src-path "<path-to-rust-srcdir>/src/")
    )
    (message "Could not load racer: missing dependencies")
  )
)
