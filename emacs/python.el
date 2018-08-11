;; pip install python-language-server[all]
(use-package lsp-python
  :ensure t
  :quelpa ((lsp-python :fetcher github :repo "emacs-lsp/lsp-python"))
  :hook (python-mode . lsp-python-enable))
