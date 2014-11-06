; require emacs 24+

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

; mouse support
(unless window-system
  ; mouse module
  (require 'mouse)
  (xterm-mouse-mode t)
  ; scroll down
  (global-set-key [mouse-4] (lambda () (interactive) (scroll-down 1)))
  ; scroll up
  (global-set-key [mouse-5] (lambda () (interactive) (scroll-up 1)))
  ; cusor move
  (defun track-mouse (e))
  (setq mouse-sel-mode t)
  )

; set the directories for backups, autosaves and sessions
(setq backup-directory-alist `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms `((".*" ,temporary-file-directory t)))
(setq auto-save-list-file-prefix temporary-file-directory)

; show lines numbers, see http://www.emacswiki.org/LineNumbers
(require 'linum)
; add a blank space after the line number
(setq linum-format "%d ")
(global-linum-mode 1)
