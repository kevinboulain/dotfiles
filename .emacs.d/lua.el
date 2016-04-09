; lua mode setup

(defconst lua-mode "~/.emacs.d/lua/")

; test if the submodule exists
(when (file-readable-p lua-mode)
  ; add it to load path
  (add-to-list 'load-path lua-mode)

  (when (require 'lua-mode nil t)
    (add-to-list 'auto-mode-alist '("\\.lua$" . lua-mode))
    (add-to-list 'interpreter-mode-alist '("lua" . lua-mode))
  )
)
