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
  ;; timestamp setup
  (setq lui-time-stamp-format "%H:%M")
  (setq lui-time-stamp-position 'right-margin))
