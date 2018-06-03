(use-package zenburn-theme
  :ensure t
  :quelpa ((zenburn-theme :fetcher github :repo "bbatsov/zenburn-emacs"))
  :init
  (setq zenburn-override-colors-alist '(("zenburn-bg" . "#1F1F1F"))))
