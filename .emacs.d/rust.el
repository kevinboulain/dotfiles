; rust mode setup

(defconst rust-mode "~/.emacs.d/rust-mode/")

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

  (when (require 'flycheck-rust nil t)
    ; add a flycheck hook
    (add-hook 'flycheck-mode-hook #'flycheck-rust-setup)
  )
)
