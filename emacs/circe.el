;;; circe.el --- IRC setup.

;;; Commentary:

;;; Some care has been taken to remove information leak.
;;;
;;; You'll probably want to setup personal.el with something similar to:
;;;  (setq lui-logging-directory "path/to/irc") ; store logs in this directory
;;;  (setq circe-network-options '(
;;;    ("freenode"
;;;     :host "irc.freenode.net"
;;;     :port 6697
;;;     :tls t
;;;     :nick "USERNAME"
;;;     :nickserv-nick "USERNAME"
;;;     :nickserv-password "PASSWORD"
;;;     :nickserv-mask "^NickServ!NickServ@services\\.$"
;;;     :nickserv-identify-challenge "\C-b/msg\\s-NickServ\\s-identify\\s-<password>\C-b"
;;;     :nickserv-identify-command "PRIVMSG NickServ :IDENTIFY {nick} {password}"
;;;     :nickserv-identify-confirmation "^You are now identified for .*\\.$"
;;;     :nickserv-ghost-command "PRIVMSG NickServ :GHOST {nick} {password}"
;;;     :nickserv-ghost-confirmation "has been ghosted\\.$\\|is not online\\.$"
;;;     :channels ("#channel1" "#channel2")
;;;    )
;;;    ("mozilla"
;;;     :host "irc.mozilla.org"
;;;     :port 6697
;;;     :tls t
;;;     :nick "USERNAME"
;;;     :channels ("#channel1" "#channel2")
;;;    )
;;;  ))

;;; Code:

(use-package circe
  :defer t
  :straight (:host github :repo "jorgenschaefer/circe")
  :hook (circe-mode . (lambda () (setq-local right-margin-width 5)))
  :config
  ;; colorize nicks
  (require 'circe-color-nicks)
  (enable-circe-color-nicks)

  ;; enable logging, may want to set lui-logging-directory (defaults to ~/.logs)
  (require 'lui-logging)
  (enable-lui-logging-globally)

  (setq lui-flyspell-p t) ; enable spell checking, see ispell.el

  (setq lui-fill-type nil ; no text wrapping
        lui-time-stamp-format "%H:%M"
        lui-time-stamp-position 'right-margin ; see above hook
        circe-format-server-topic "*** Topic change by {nick} ({userhost}): {topic-diff}") ; topic-diff instead of new-topic

  ;; log a new day
  (require 'circe-new-day-notifier)
  (enable-circe-new-day-notifier)
  (add-to-list 'circe-format-not-tracked 'circe-new-day-notifier-format-message) ; don't track day notifications

  ;; anonymization
  (setq circe-default-quit-message ""
        circe-default-part-message ""
        circe-default-realname circe-default-nick)) ; avoid real identity leak from user-full-name

;;; circe.el ends here
