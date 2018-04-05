(defconst avy (concat user-emacs-directory "avy"))

(when (file-readable-p avy)
  (add-to-list 'load-path avy)

  (when (require 'avy nil t)
    ; I think I never used open-line, so override default binding
    (global-set-key (kbd "C-o") 'avy-goto-char)
  )
)
