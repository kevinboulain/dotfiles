; some modules to load
(add-to-list 'load-path "~/.emacs.d/os/darwin/")

; mouse support, for now not really useful?
(unless window-system
  ; mouse module
  (require 'mouse)
  (xterm-mouse-mode t)
  (global-set-key [mouse-4] (lambda () (interactive) (scroll-down 1)))
  (global-set-key [mouse-5] (lambda () (interactive) (scroll-up 1)))
  (defun track-mouse (e))
  (setq mouse-sel-mode t)
)
