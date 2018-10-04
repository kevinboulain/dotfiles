;;; ethan-wspace.el --- Highlight incorrect use of whitespace.

;;; Commentary:

;;; Reponsible for highlighting tabulations, trailing spaces, etc.
;;; When the file is considered sane, it will automatically clean up on save.

;;; Code:

;; whitespace-mode faces mess up with actual text properties (for example, in circe)
;; ethan-wspace seems to aim to do better
(use-package ethan-wspace
  :straight (:host github :repo "glasserc/ethan-wspace")
  :config
  (setq mode-require-final-newline nil) ; don't automatically add final newlines
  (global-ethan-wspace-mode 1))

;;; ethan-wspace.el ends here
