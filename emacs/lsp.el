(use-package lsp-mode
  :ensure t
  :quelpa ((lsp-mode :fetcher github :repo "emacs-lsp/lsp-mode")))

(use-package lsp-ui
  :ensure t
  :quelpa ((lsp-ui :fetcher github :repo "emacs-lsp/lsp-ui"))
  :hook (lsp-mode . lsp-ui-mode))

(use-package company-lsp
  :ensure t
  :quelpa ((company-lsp :fetcher github :repo "tigersoldier/company-lsp"))
  :config
  (push 'company-lsp company-backends))
