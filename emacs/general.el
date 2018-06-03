;; tab size
(setq-default tab-width 2)

;; indent with spaces only
(setq-default indent-tabs-mode nil)

;; set the directories for backups, autosaves and sessions
(setq backup-directory-alist `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms `((".*" ,temporary-file-directory t)))
(setq auto-save-list-file-prefix temporary-file-directory)

;; 'commander' interface for dired
(setq dired-dwim-target t)

;; annoying questions 'yes' or 'no'
(defalias 'yes-or-no-p 'y-or-n-p)

;; disable source control handling
(setq vc-handled-backends nil)

;; show pointer's current column and line
(setq line-number-mode t)
(setq column-number-mode t)

;; remove the menu (f10)
(menu-bar-mode -1)

;; remove the \ for a wrapped line
(set-display-table-slot standard-display-table 'wrap ?\ )

;; default keybindings for windmove: shift + arrows
(windmove-default-keybindings)

;; show matching parenthesis
(show-paren-mode 1)

;; split horizontally by default
(setq split-width-threshold 1)

;; enable better buffer/file completion
(when (require 'ido nil t)
  (setq ido-enable-flex-matching t)
  (ido-mode t)
  ;; disable annoying directory search
  (setq ido-auto-merge-work-directories-length -1))

;; don't show the 'GNU Emacs' buffer
(setq inhibit-startup-screen t)

;; mouse support
(unless window-system
  (when (require 'mouse nil t)
    (xterm-mouse-mode t)
    (global-set-key [mouse-4] (lambda () (interactive) (scroll-down 1)))
    (global-set-key [mouse-5] (lambda () (interactive) (scroll-up 1)))))
