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

(use-package lsp-clangd
  :ensure t
  :quelpa ((lsp-clangd :fetcher github :repo "emacs-lsp/lsp-clangd"))
  :hook ((c-mode . lsp-clangd-c-enable)
         (c++-mode . lsp-clangd-c++-enable)))
