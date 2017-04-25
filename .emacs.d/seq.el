(defconst seq "~/.emacs.d/seq/")

(when (file-readable-p seq)
  (add-to-list 'load-path seq)

  (when (require 'seq nil t)
  )
)
