(use-package flycheck
  :ensure t
  :quelpa ((flycheck :fetcher github :repo "flycheck/flycheck"))
  :hook (prog-mode . flycheck-mode)
  :config
  (setq flycheck-checker-error-threshold nil))
