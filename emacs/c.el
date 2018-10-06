;;; c.el --- C language support.

;;; Commentary:

;;; Code:

(c-set-offset 'case-label '+) ; indent case in switch
(setq c-basic-offset tab-width
      c-default-style "k&r")

(when (require 'clang-format nil t)
  ;; (global-set-key [C-M-tab] 'clang-format-region)
  )

;; you'll need to build cquery, but it seems better than lsp-clangd
;; https://github.com/cquery-project/cquery/wiki/Emacs
(use-package cquery
  :straight (:host github :repo "cquery-project/emacs-cquery")
  :hook ((c-mode . lsp-cquery-enable)
         (c++-mode . lsp-cquery-enable)))

;; straight not supporting shallow clones,
;; use a mirror instead of the official cmake repository
;; mainly for cmake-help*
(use-package cmake-mode
  :straight (:host github :repo "emacsmirror/cmake-mode"))

;;; c.el ends here
