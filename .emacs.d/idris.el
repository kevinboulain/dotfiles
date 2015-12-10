; idris mode setup

(defconst idris-mode "~/.emacs.d/idris-mode/")

; test if the submodule exists
(when (file-readable-p idris-mode)
  ; add it to load path
  (add-to-list 'load-path idris-mode)

  (when (require 'idris-mode nil t)
  )
)
