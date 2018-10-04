;;; rust.el --- Rust language support.

;;; Commentary:

;;; Code:

(use-package rust-mode
  :defer t
  :straight (:host github :repo "rust-lang/rust-mode")
  :config
  (add-to-list 'auto-mode-alist '("\\.rs$" . rust-mode))
  (setq rust-indent-offset tab-width))

(use-package flycheck-rust
  :straight (:host github :repo "flycheck/flycheck-rust")
  :hook (flycheck-mode . flycheck-rust-setup))

(use-package lsp-rust
  :straight (:host github :repo "emacs-lsp/lsp-rust")
  :hook (rust-mode . lsp-rust-enable))

;;; rust.el ends here
