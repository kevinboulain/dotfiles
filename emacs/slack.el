;; ouath2 seems kinda broken
(use-package oauth2
  :defer t
  :quelpa ((oauth2 :fetcher github :repo "emacsmirror/oauth2"))
  :init
  ;; avoid warnings, :defines doesn't seem to work
  (defvar oauth--token-data ())
  (defvar url-callback-arguments ())
  (defvar url-callback-function ())
  (defvar url-http-data ())
  (defvar url-http-extra-headers ())
  (defvar url-http-method ()))

(use-package slack
  :defer t
  :quelpa ((slack :fetcher github :repo "yuya373/emacs-slack"))
  :hook (slack-mode . (lambda () (setq-local right-margin-width 5)))
  :config
  ;; the lazy way is to search the network tab for '_x_id' & 'xoxs-'
  ;; it's probably better to create an application
  ;; (slack-register-team
  ;;   :name "team"
  ;;   :default t
  ;;   :client-id "_x_id"
  ;;   :client-secret "account's password"
  ;;   :token "xoxs-"
  ;;   :full-and-display-names t
  ;; )
  ;; timestamp setup
  (setq lui-time-stamp-format "%H:%M")
  (setq lui-time-stamp-position 'right-margin))
