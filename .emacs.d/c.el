; cscope module

(defvar xcscope "~/.emacs.d/xcscope/")

; test if the submodule exists
(when (file-readable-p xcscope)
  (add-to-list 'load-path xcscope)

  ; activate module
  (when (require 'xcscope nil t)
    (cscope-setup)
  )
)

; c mode indentation
;(setq c-basic-offset tab-width)
