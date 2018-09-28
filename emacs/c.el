(when (require 'cc-mode nil t)
  (setq c-basic-offset tab-width)
  (setq c-default-style "k&r")
  (c-set-offset 'arglist-intro '+)
  (c-set-offset 'arglist-close 0)
  (c-set-offset 'case-label '+)
  ;; (c-toggle-auto-state 1)
  ;; (c-toggle-hungry-state 1)
  )

(when (require 'clang-format nil t)
  ;; (global-set-key [C-M-tab] 'clang-format-region)
  )

;; you'll need to build cquery, but it seems better than lsp-clangd
;; https://github.com/cquery-project/cquery/wiki/Emacs
(use-package cquery
  :straight (:host github :repo "cquery-project/emacs-cquery")
  :hook ((c-mode . lsp-cquery-enable)
         (c++-mode . lsp-cquery-enable)))
