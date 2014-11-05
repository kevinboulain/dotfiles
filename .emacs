; monokai theme
(add-to-list 'custom-theme-load-path "~/.emacs.d/monokai-emacs/")
(load-theme 'monokai t)

; whitespace module
(require 'whitespace)
; highlight 80+ columns
(setq whitespace-line-column 80)
; what to display, C-h v whitespace-style to see possibilites
(setq whitespace-style '(face lines-tail trailing empty)) ; space-mark tab-mark
; activate module
(global-whitespace-mode t)

; show pointer column and line
(setq line-number-mode t)
(setq column-number-mode t)

; remove the menu (f10)
(menu-bar-mode -1)

; indent with spaces only
(setq-default indent-tabs-mode nil)

; remove the \ for a wrapped line
(set-display-table-slot standard-display-table 'wrap ?\ )

; mouse support, not practical for copy/paste
;(require 'mouse)
;(xterm-mouse-mode t)
;(defun track-mouse (e))
