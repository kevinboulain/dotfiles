; idris-mode dependency
(defconst prop-menu (concat user-emacs-directory "prop-menu"))

(when (file-readable-p prop-menu)
  (add-to-list 'load-path prop-menu)

  (when (require 'prop-menu nil t)
  )
)

(defconst idris-mode (concat user-emacs-directory "idris"))

(when (file-readable-p idris-mode)
  (add-to-list 'load-path idris-mode)

  (if (featurep 'prop-menu)
    (when (require 'idris-mode nil t)
      ; disable startup animation
      (setq idris-repl-banner-functions nil)
    )
    (message "Could not load idris-mode: missing dependencies")
  )
)
