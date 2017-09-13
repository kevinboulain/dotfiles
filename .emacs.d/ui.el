; emacs 24 specific stuff
(when (>= emacs-major-version 24)
  ; monokai
  (add-to-list 'custom-theme-load-path (concat user-emacs-directory "theme/monokai"))
  ; zenburn
  (add-to-list 'custom-theme-load-path (concat user-emacs-directory "theme/zenburn"))
  (defconst zenburn-override-colors-alist '(
    ("zenburn-bg" . "#1F1F1F")
  ))
  ; tao
  (defconst tao-theme (concat user-emacs-directory "theme/tao"))
  (add-to-list 'custom-theme-load-path tao-theme)

  (cond ; stop at the first available theme
    ((member 'monokai (custom-available-themes)) (load-theme 'monokai t))
    ((member 'zenburn (custom-available-themes)) (load-theme 'zenburn t))
    ((member 'tao-yin (custom-available-themes)) (add-to-list 'load-path tao-theme) (load-theme 'tao-yin t))
  )
)

; whitespace module
(when (require 'whitespace nil t)
  (setq whitespace-line-column 80) ; highlight 80+ columns, this is the default
  (setq whitespace-style
    '(face ; visual impact
      trailing empty ; everything that has too much blank
      ; lines-tail ; highlight lines with more than whitespace-line-column columns
      ; tabs spaces ; show tabs and spaces with faces
      ; tab-mark space-mark ; show tabs and spaces with markers
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

; fast deletion
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

; enable better buffer/file completion
(when (require 'ido nil t)
  (setq ido-enable-flex-matching t)
  (ido-mode t)
  (setq ido-auto-merge-work-directories-length -1) ; disable annoying directory search
)
