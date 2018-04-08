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

; show pointer's current column and line
(setq line-number-mode t)
(setq column-number-mode t)

; remove the menu (f10)
(menu-bar-mode -1)

; remove the \ for a wrapped line
(set-display-table-slot standard-display-table 'wrap ?\ )

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
; (setq pop-up-windows nil)

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

; don't show the 'GNU Emacs' buffer
(setq inhibit-startup-screen t)

; mouse support
(unless window-system
  (when (require 'mouse nil t)
    (xterm-mouse-mode t)
    (global-set-key [mouse-4] (lambda () (interactive) (scroll-down 1)))
    (global-set-key [mouse-5] (lambda () (interactive) (scroll-up 1)))
  )
)
