; tab completion

(defvar smart-tab "~/.emacs.d/smart-tab/")

; test if the submodule exists
(when (file-readable-p smart-tab)
  ; add it to load path
  (add-to-list 'load-path smart-tab)

  (when (require 'smart-tab nil t)
    ; activate smart-tab
    (global-smart-tab-mode 1)
    ; smart-tab use dabbrev-mode, make dabbrev-mode case sensitive
    (setq dabbrev-case-fold-search nil)
  )
)
