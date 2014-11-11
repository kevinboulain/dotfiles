; some modules to load
(add-to-list 'load-path "~/.emacs.d/modules/")

; emacs 24 specific stuff
(when (>= emacs-major-version 24)
  ; monokai theme
  (add-to-list 'custom-theme-load-path "~/.emacs.d/monokai/")
  (load-theme 'monokai t)

  ; show lines numbers, see http://www.emacswiki.org/LineNumbers
  (require 'linum)
  (setq linum-format "%d ") ; add a blank space after the line number
  (global-linum-mode 1)
  )

; Mac OS X specific stuff
(when (eq system-type 'darwin)
  ; (turn-on-pbcopy) done via terminal-init-xterm-hook
  (require 'pbcopy)

  ; enable gud (lldb debugger in emacs mode)
  (require 'gud)
  )

; mouse support
(unless window-system
  ; mouse module
  (require 'mouse)
  (xterm-mouse-mode t)
  (global-set-key [mouse-4] (lambda () (interactive) (scroll-down 1)))
  (global-set-key [mouse-5] (lambda () (interactive) (scroll-up 1)))
  (defun track-mouse (e))
  (setq mouse-sel-mode t)
  )

; cscope module
(add-to-list 'load-path "~/.emacs.d/xcscope/")
(require 'xcscope)
(cscope-setup) ; activate module

; whitespace module
(require 'whitespace)
(setq whitespace-line-column 80) ; highlight 80+ columns, this is the default
(setq whitespace-style '(face lines-tail trailing empty)) ; what to display
(global-whitespace-mode t) ; activate module

; show pointer's current column and line
(setq line-number-mode t)
(setq column-number-mode t)

; remove the menu (f10)
(menu-bar-mode -1)

; indent with spaces only
(setq-default indent-tabs-mode nil)

; remove the \ for a wrapped line
(set-display-table-slot standard-display-table 'wrap ?\ )

; set the directories for backups, autosaves and sessions
(setq backup-directory-alist `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms `((".*" ,temporary-file-directory t)))
(setq auto-save-list-file-prefix temporary-file-directory)

; window resizing
(global-set-key (kbd "ESC <left>") 'shrink-window-horizontally)
(global-set-key (kbd "ESC <right>") 'enlarge-window-horizontally)
(global-set-key (kbd "ESC <down>") 'shrink-window)
(global-set-key (kbd "ESC <up>") 'enlarge-window)
