; tab size
(setq-default tab-width 2)

; indent with spaces only
(setq-default indent-tabs-mode nil)

; set the directories for backups, autosaves and sessions
(setq backup-directory-alist `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms `((".*" ,temporary-file-directory t)))
(setq auto-save-list-file-prefix temporary-file-directory)

; 'commander' interface for dired
(setq dired-dwim-target t)

; annoying questions 'yes' or 'no'
(defalias 'yes-or-no-p 'y-or-n-p)

; disable source control handling
(setq vc-handled-backends nil)
