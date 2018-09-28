(use-package company
  :straight (:host github :repo "company-mode/company-mode")
  :hook (after-init . global-company-mode)
  :config
  ;; dabbrev complete case sensitive
  (setq company-dabbrev-downcase nil)
  ;; bind shit+tab on company complete
  ;; (global-set-key (kbd "<backtab>") 'company-complete)
  ;; no delay before showing completion
  (setq company-idle-delay 0)
  ;; start completing after the first typed character
  (setq company-minimum-prefix-length 2)
  ;; alignment
  (setq company-tooltip-align-annotations t))
