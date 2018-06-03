(use-package idris-mode
  :ensure t
  :quelpa ((idris-mode :fetcher github :repo "idris-hackers/idris-mode"))
  :config
  ;; disable startup animation
  (setq idris-repl-banner-functions nil))
