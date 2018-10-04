;;; company.el --- Basic autocompletion setup.

;;; Commentary:

;;; Code:

(use-package company
  :straight (:host github :repo "company-mode/company-mode")
  :hook (after-init . global-company-mode)
  :config
  (setq company-dabbrev-downcase nil ; dabbrev complete case sensitive
        company-idle-delay 0 ; no delay before showing completion
        company-minimum-prefix-length 2 ; start completing rapidly
        company-tooltip-align-annotations t)) ; align annotations to the right

;;; company.el ends here
