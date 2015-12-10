; seq module

(defconst seq "~/.emacs.d/seq/")

; test if the submodule exists
(when (file-readable-p seq)
  (add-to-list 'load-path seq)

  ; activate module
  (when (require 'seq nil t)
  )
)
