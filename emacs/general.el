;;; general.el --- General Emacs setup.

;;; Commentary:

;;; Configure builtin stuff.

;;; Code:

;; indentation
(setq-default tab-width 2 ; tab size
              indent-tabs-mode nil) ; indent with spaces only

;; set the directories for backups, autosaves and sessions
(setq backup-directory-alist `((".*" . ,temporary-file-directory))
      auto-save-file-name-transforms `((".*" ,temporary-file-directory t))
      auto-save-list-file-prefix temporary-file-directory)

(setq dired-dwim-target t) ; 'commander' interface for dired

(defalias 'yes-or-no-p 'y-or-n-p) ; annoying questions 'yes' or 'no'

(setq vc-handled-backends nil) ; disable source control handling

;; better pointer reporting
(line-number-mode 1) ; show line number in the mode-line
(column-number-mode 1) ; show column number in the mode-line
(global-hl-line-mode) ; highlight the line containing the cursor

;; recursive minibuffer
(setq enable-recursive-minibuffers t)
(minibuffer-depth-indicate-mode 1)

(menu-bar-mode -1) ; remove the menu (f10)

(set-display-table-slot standard-display-table 'wrap ?\ ) ; remove the \ for a wrapped line

(windmove-default-keybindings) ; default keybindings for windmove: shift + arrows

(show-paren-mode 1) ; show matching parenthesis

(setq split-width-threshold 1) ; split horizontally by default

(setq inhibit-startup-screen t) ; don't show the 'GNU Emacs' buffer
;; to avoid displaying the 'For information about GNU Emacs...' in the minibuffer
;; inhibit-startup-echo-area-message must be hardcoded in the init file to your username...

(setq load-prefer-newer t) ; always prefer newer source files

;; mouse support
(unless window-system
  (when (require 'mouse nil t)
    (xterm-mouse-mode t)
    (global-set-key [mouse-4] (lambda () (interactive) (scroll-down 1)))
    (global-set-key [mouse-5] (lambda () (interactive) (scroll-up 1)))))

;;; general.el ends here
