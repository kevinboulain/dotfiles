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

; whitespace module
(require 'whitespace)
(setq whitespace-line-column 80) ; highlight 80+ columns, this is the default
(setq whitespace-style
  '(face ; visual impact
    lines-tail trailing empty ; everything that have too much blank
    ;tabs tab-mark spaces space-mark ; show tabs and spaces with faces
  )
) ; C-h v whitespace-style to display possibilites
(global-whitespace-mode t) ; activate module

; show pointer's current column and line
(setq line-number-mode t)
(setq column-number-mode t)

; remove the menu (f10)
(menu-bar-mode -1)

; remove the \ for a wrapped line
(set-display-table-slot standard-display-table 'wrap ?\ )

; show time
; emacs does not refresh the screen for %S
(setq display-time-format "%H:%M %d/%m/%Y")
(display-time-mode 1)

; window resizing
(global-set-key (kbd "ESC <left>") 'shrink-window-horizontally)
(global-set-key (kbd "ESC <right>") 'enlarge-window-horizontally)
(global-set-key (kbd "ESC <down>") 'shrink-window)
(global-set-key (kbd "ESC <up>") 'enlarge-window)
