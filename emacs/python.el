(use-package lsp-python
  :ensure t
  :quelpa ((lsp-python :fetcher github :repo "emacs-lsp/lsp-python"))
  :hook (python-mode . lsp-python-enable))
