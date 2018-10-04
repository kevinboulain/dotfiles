;;; idris.el --- Idris language support.

;;; Commentary:

;;; Code:

(use-package idris-mode
  :defer t
  :straight (:host github :repo "idris-hackers/idris-mode")
  :config
  (setq idris-repl-banner-functions nil)) ; disable startup animation

;;; idris.el ends here
