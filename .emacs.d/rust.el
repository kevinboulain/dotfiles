; rust mode setup

(defvar rust-mode "~/.emacs.d/rust-mode/")

; test if the submodule exists
(when (file-readable-p rust-mode)
  ; add it to load path
  (add-to-list 'load-path rust-mode)

  (when (require 'rust-mode nil t)
    (add-to-list 'auto-mode-alist '("\\.rs\\'" . rust-mode))
  )
)
