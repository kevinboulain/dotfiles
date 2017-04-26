(defconst xcscope (concat user-emacs-directory "xcscope"))

(when (file-readable-p xcscope)
  (add-to-list 'load-path xcscope)

  (when (require 'xcscope nil t)
    (cscope-setup)
  )
)
