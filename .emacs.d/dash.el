; dash module

(defconst dash "~/.emacs.d/dash/")

; test if the submodule exists
(when (file-readable-p dash)
  (add-to-list 'load-path dash)

  ; activate module
  (when (require 'dash nil t)
  )
)
