(use-package idris-mode
  :defer t
  :straight (:host github :repo "idris-hackers/idris-mode")
  :config
  ;; disable startup animation
  (setq idris-repl-banner-functions nil))
