(use-package circe
  :ensure t
  :defer t
  :quelpa ((circe :fetcher github :repo "jorgenschaefer/circe"))
  :hook (circe-mode . (lambda () (setq-local right-margin-width 5)))
  :config
  ;; colorize nicks
  (require 'circe-color-nicks)
  (enable-circe-color-nicks)
  ;; enable logging, may want to set lui-logging-directory (defaults to ~/.logs)
  (require 'lui-logging)
  (enable-lui-logging-globally)
  ;; no text wrapping
  (setq lui-fill-type nil)
  ;; enable spell checking, see spellcheck.el
  (setq lui-flyspell-p t)
  ;; timestamp setup
  (setq lui-time-stamp-format "%H:%M")
  (setq lui-time-stamp-position 'right-margin)
  ;; log a new day
  (require 'circe-new-day-notifier)
  (enable-circe-new-day-notifier)
  ;; don't track day notifications
  (add-to-list 'circe-format-not-tracked 'circe-new-day-notifier-format-message)
  ;; topic-diff instead of new-topic
  (setq circe-format-server-topic "*** Topic change by {nick} ({userhost}): {topic-diff}")
  ;; anonymization
  (setq circe-default-quit-message "")
  (setq circe-default-part-message "")
  ;; change that one or simply comment it out, avoid possible identity leak from user-login-name
  (setq circe-default-nick "ether")
  ;; reset to circe-default-nick
  (setq circe-default-user circe-default-nick)
  ;; avoid real identity from user-full-name
  (setq circe-default-realname circe-default-nick))
