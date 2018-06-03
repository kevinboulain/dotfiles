(use-package which-key
  :ensure t
  :quelpa ((which-key :fetcher github :repo "justbur/emacs-which-key"))
  :config
  (setq which-key-separator " ")
  (which-key-mode))
