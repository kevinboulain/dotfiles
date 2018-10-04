;;; which-key.el --- Display keybinding hints.

;;; Commentary:

;;; Code:

(use-package which-key
  :straight (:host github :repo "justbur/emacs-which-key")
  :config
  (setq which-key-separator " ")
  (which-key-mode))

;;; which-key.el ends here
