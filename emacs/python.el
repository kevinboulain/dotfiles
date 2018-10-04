;;; python.el --- Python language support.

;;; Commentary:

;;; Install the language server with:
;;;  pip install python-language-server[all]

;;; Code:

(use-package lsp-python
  :straight (:host github :repo "emacs-lsp/lsp-python")
  :hook (python-mode . lsp-python-enable))

;;; python.el ends here
