(defconst s "~/.emacs.d/s/")

(when (file-readable-p s)
  (add-to-list 'load-path s)

  (when (require 's nil t)
  )
)
