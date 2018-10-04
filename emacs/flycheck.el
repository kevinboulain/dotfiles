;;; flycheck.el --- Basic flycheck setup.

;;; Commentary:

;;; Code:

(use-package flycheck
  :straight (:host github :repo "flycheck/flycheck")
  :hook (prog-mode . flycheck-mode)
  :config
  (setq flycheck-checker-error-threshold nil ; don't stop after a large number of errors
        flycheck-temp-prefix ".flycheck")) ; hide temporary files

;; flycheck.el ends here
