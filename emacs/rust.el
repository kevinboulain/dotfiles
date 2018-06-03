(use-package rust-mode
  :ensure t
  :quelpa ((rust-mode :fetcher github :repo "rust-lang/rust-mode"))
  :config
  (add-to-list 'auto-mode-alist '("\\.rs$" . rust-mode))
  (setq rust-indent-offset tab-width))

(use-package flycheck-rust
  :ensure t
  :quelpa ((flycheck-rust :fetcher github :repo "flycheck/flycheck-rust"))
  :hook (flycheck-mode . flycheck-rust-setup))

(use-package lsp-rust
  :ensure t
  :quelpa ((lsp-rust :fetcher github :repo "emacs-lsp/lsp-rust"))
  :hook (rust-mode . lsp-rust-enable))
