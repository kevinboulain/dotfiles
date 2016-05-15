; s module

(defconst s "~/.emacs.d/s/")

; test if the submodule exists
(when (file-readable-p s)
  (add-to-list 'load-path s)

  ; activate module
  (when (require 's nil t)
  )
)
