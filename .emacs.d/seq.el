(defconst seq (concat user-emacs-directory "seq"))

(when (file-readable-p seq)
  (add-to-list 'load-path seq)

  (when (require 'seq nil t)
  )
)
