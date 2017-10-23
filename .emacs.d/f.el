(defconst f (concat user-emacs-directory "f"))

(when (file-readable-p f)
  (add-to-list 'load-path f)

  (if (featurep 's)
    (when (require 'f nil t)
    )
    (message "Could not load f: missing dependencies")
  )
)
