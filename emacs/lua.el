;;; lua.el --- Lua language support.

;;; Commentary:

;;; Code:

(use-package lua-mode
  :defer t
  :straight (:host github :repo "immerrr/lua-mode")
  :config
  (autoload 'lua-mode "lua-mode" "Lua editing mode." t)
  (add-to-list 'auto-mode-alist '("\\.lua$" . lua-mode))
  (add-to-list 'interpreter-mode-alist '("lua" . lua-mode)))

;;; lua.el ends here
