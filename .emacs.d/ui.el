; emacs 24 specific stuff
(when (>= emacs-major-version 24)
  ; monokai theme
  (add-to-list 'custom-theme-load-path "~/.emacs.d/monokai/")
  (when (member 'monokai (custom-available-themes))
    (load-theme 'monokai t)
  )
)

; whitespace module
(when (require 'whitespace nil t)
  (setq whitespace-line-column 80) ; highlight 80+ columns, this is the default
  (setq whitespace-style
    '(face ; visual impact
      lines-tail trailing empty ; everything that have too much blank
      ;tabs tab-mark spaces space-mark ; show tabs and spaces with faces
    )
  ) ; C-h v whitespace-style to display possibilites
  (global-whitespace-mode t) ; activate module
)

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

; window resizing, use ESC \d+ to repeat
(global-set-key (kbd "ESC <left>") 'shrink-window-horizontally)
(global-set-key (kbd "ESC <right>") 'enlarge-window-horizontally)
(global-set-key (kbd "ESC <down>") 'shrink-window)
(global-set-key (kbd "ESC <up>") 'enlarge-window)

(global-set-key (kbd "C-<delete>") 'c-hungry-delete-forward)

; toggle window dedication
(defun toggle-window-dedicated ()
  (interactive)
  (message
    (if (let (window (get-buffer-window (current-buffer)))
          (set-window-dedicated-p window (not (window-dedicated-p window))))
      "Window is dedicated to buffer '%s'"
      "Window is no more dedicated to buffer '%s'")
    (current-buffer)
  )
)
(global-set-key (kbd "C-!") 'toggle-window-dedicated)

; disallow emacs from opening new pop up windows
(setq pop-up-windows nil)

; default keybindings for windmove: shift + arrows
(windmove-default-keybindings)

; show matching parenthesis
(show-paren-mode 1)

; split horizontally by default
(setq split-width-threshold 1)
