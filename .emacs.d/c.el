; cscope module
(add-to-list 'load-path "~/.emacs.d/xcscope/")
(require 'xcscope)
; activate module
(cscope-setup)

; c mode indentation
(setq c-basic-offset default-tab-width)
