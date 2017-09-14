; allow easy discovery of available shortcuts by showing completions of unfinished commands
; ESC seems to list one-level modifiers

(defconst irc (concat user-emacs-directory "which-key"))

(when (file-readable-p irc)
  (add-to-list 'load-path irc)

  (when (require 'which-key nil t)
    (setq which-key-separator " ")
    (which-key-mode)
  )
)
