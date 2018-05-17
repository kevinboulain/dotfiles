(defconst alert (concat user-emacs-directory "alert"))

(when (file-readable-p alert)
  (add-to-list 'load-path alert)

  (when (require 'alert nil t)
  )
)

(defconst oauth2 (concat user-emacs-directory "oauth2"))

(when (file-readable-p oauth2)
  (add-to-list 'load-path oauth2)

  ; avoid warnings
  (defvar url-http-extra-headers ())
  (defvar oauth--token-data ())
  (defvar url-callback-function ())
  (defvar url-callback-arguments ())
  (when (require 'oauth2 nil t)
  )
)

(defconst request (concat user-emacs-directory "request"))

(when (file-readable-p request)
  (add-to-list 'load-path request)

  (when (require 'request nil t)
  )
)

(defconst websocket (concat user-emacs-directory "websocket"))

(when (file-readable-p websocket)
  (add-to-list 'load-path websocket)

  (when (require 'websocket nil t)
  )
)

(defconst slack (concat user-emacs-directory "slack"))

(when (file-readable-p slack)
  (add-to-list 'load-path slack)

  (if (and (featurep 'alert)
           (featurep 'circe)
           (featurep 'oauth2)
           (featurep 'request)
           (featurep 'websocket))
    (when (require 'slack nil t)
      ; timestamp setup
      ; copy/pasted from irc.el
      (setq lui-time-stamp-format "%H:%M")
      (setq lui-time-stamp-position 'right-margin)
      (add-hook 'slack-mode-hook (lambda ()
        (setq-local right-margin-width 5)
      ))
    )
    (message "Could not load slack: missing dependencies")
  )
)
