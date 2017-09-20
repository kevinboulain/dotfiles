; could probably check out the following issues with hl-line:
;  - prompt background
;  - right margin highlighting (hosting the timestamp)
; should also check for circe's markers
; see Freenode example in ./circe/circe.el (put it in ./personal.el)

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
    ; log a new day
    (require 'circe-new-day-notifier)
    (enable-circe-new-day-notifier)
    (add-to-list 'circe-format-not-tracked 'circe-new-day-notifier-format-message) ; don't track day notifications

    ; topic-diff instead of new-topic
    (setq circe-format-server-topic "*** Topic change by {nick} ({userhost}): {topic-diff}")

    ; prompt setup (irssi-like)
    ; (add-hook 'circe-chat-mode-hook (lambda ()
    ;   (lui-set-prompt (concat "[" (buffer-name) "] "))
    ; ))

    ; anonymization
    (setq circe-default-quit-message "")
    (setq circe-default-part-message "")
    (setq circe-default-nick "ether") ; change that one or simply comment it out, avoid possible identity leak from user-login-name
    (setq circe-default-user circe-default-nick) ; reset to circe-default-nick
    (setq circe-default-realname circe-default-nick) ; avoid real identity from user-full-name
  )
)
