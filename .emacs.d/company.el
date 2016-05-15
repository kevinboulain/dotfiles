; company module

(defconst company "~/.emacs.d/company/")

; test if the submodule exists
(when (file-readable-p company)
  (add-to-list 'load-path company)

  ; activate module
  (when (require 'company nil t)
    (add-hook 'after-init-hook 'global-company-mode)
    ; dabbrev complete case sensitive
    (setq company-dabbrev-downcase nil)
    ; bind shit+tab on company complete
    (global-set-key (kbd "<backtab>") 'company-complete)
    ; no delay before showing completion
    (setq company-idle-delay 0)
    ; start completing after the first typed character
    (setq company-minimum-prefix-length 1)
  )
)
