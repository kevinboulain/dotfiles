(defconst rainbow-delimiters (concat user-emacs-directory "rainbow-delimiters"))

(when (file-readable-p rainbow-delimiters)
  (add-to-list 'load-path rainbow-delimiters)

  (when (require 'rainbow-delimiters nil t)
    (add-hook 'prog-mode-hook #'rainbow-delimiters-mode)
    (add-hook 'text-mode-hook #'rainbow-delimiters-mode)
  )
)
