(defconst dash "~/.emacs.d/dash/")

(when (file-readable-p dash)
  (add-to-list 'load-path dash)

  (when (require 'dash nil t)
  )
)
