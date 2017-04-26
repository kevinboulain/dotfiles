(defconst htmlize (concat user-emacs-directory "htmlize"))

(when (file-readable-p htmlize)
  (add-to-list 'load-path htmlize)

  (when (require 'htmlize nil t)
  )
)
