;; pip install python-language-server[all]
(use-package lsp-python
  :straight (:host github :repo "emacs-lsp/lsp-python")
  :hook (python-mode . lsp-python-enable))
