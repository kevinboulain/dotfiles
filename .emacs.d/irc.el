; could probably check out the following issues with hl-line:
;  - prompt background
;  - right margin highlighting (hosting the timestamp)
; should also check for circe's markers

(defconst irc (concat user-emacs-directory "circe"))

(when (file-readable-p irc)
  (add-to-list 'load-path irc)

  (when (require 'circe nil t)
    ; colorize nicks
    (require 'circe-color-nicks)
    (enable-circe-color-nicks)

    ; enable logging, may want to set lui-logging-directory (defaults to ~/.logs)
    (require 'lui-logging)
    (enable-lui-logging-globally)

    (setq lui-fill-type nil) ; no text wrapping
    (setq lui-flyspell-p t) ; enable spell checking, see spellcheck.el

    ; timestamp setup
    (setq lui-time-stamp-format "%H:%M")
    (setq lui-time-stamp-position 'right-margin)
    (add-hook 'circe-mode-hook (lambda ()
      (setq-local right-margin-width 5)
    ))

    ; prompt setup
    (add-hook 'circe-chat-mode-hook (lambda ()
      (lui-set-prompt (concat "[" (buffer-name) "] "))
    ))

    ; anonymization
    (setq circe-default-quit-message "")
    (setq circe-default-part-message "")
    (setq circe-default-realname circe-default-nick) ; avoid real identity from user-full-name
  )
)
