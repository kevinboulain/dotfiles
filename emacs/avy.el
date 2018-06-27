(use-package avy
  :ensure t
  :quelpa ((avy :fetcher github :repo "abo-abo/avy"))
  :config
  ;; I think I never used open-line, so override default binding
  (global-set-key (kbd "C-o") 'avy-goto-char-2))
