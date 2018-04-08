; allow easy discovery of available shortcuts by showing completions of unfinished commands
; ESC seems to list one-level modifiers

(defconst which-key (concat user-emacs-directory "which-key"))

(when (file-readable-p which-key)
  (add-to-list 'load-path which-key)

  (when (require 'which-key nil t)
    (setq which-key-separator " ")
    (which-key-mode)
  )
)
