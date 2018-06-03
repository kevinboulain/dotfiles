(use-package lua-mode
  :ensure t
  :quelpa ((lua-mode :fetcher github :repo "immerrr/lua-mode"))
  :config
  (autoload 'lua-mode "lua-mode" "Lua editing mode." t)
  (add-to-list 'auto-mode-alist '("\\.lua$" . lua-mode))
  (add-to-list 'interpreter-mode-alist '("lua" . lua-mode)))
