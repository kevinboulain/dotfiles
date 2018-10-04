;;; slack.el --- Slack setup.

;;; Commentary:

;;; You'll probably want to setup personal.el with something similar to:
;;;  (slack-register-team
;;;    :name "team"
;;;    :default t
;;;    :client-id "_X_ID"
;;;    :client-secret "PASSWORD"
;;;    :token "XOXS-"
;;;    :full-and-display-names t
;;;  )
;;;
;;; The easiest way is to fill above information is to open the network tab
;;; and search for '_x_id' & 'xoxs-'.
;;; Or you may want to create an application.

;;; Code:

(use-package slack
  :defer t
  :straight (:host github :repo "yuya373/emacs-slack")
  :hook (slack-mode . (lambda () (setq-local right-margin-width 5)))
  :config
  ;; timestamp setup, same as in circe.el
  (setq lui-time-stamp-format "%H:%M"
        lui-time-stamp-position 'right-margin))

;;; slack.el ends here
