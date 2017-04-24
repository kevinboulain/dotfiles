; htmlize module

(defconst htmlize "~/.emacs.d/htmlize/")

; test if the submodule exists
(when (file-readable-p htmlize)
  (add-to-list 'load-path htmlize)

  ; activate module
  (when (require 'htmlize nil t)
  )
)
