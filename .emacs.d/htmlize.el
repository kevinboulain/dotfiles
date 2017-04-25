(defconst htmlize "~/.emacs.d/htmlize/")

(when (file-readable-p htmlize)
  (add-to-list 'load-path htmlize)

  (when (require 'htmlize nil t)
  )
)
