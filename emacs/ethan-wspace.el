;; whitespace-mode faces mess up with actual text properties (for example, in circe)
;; ethan-wspace seems to aim to do better
(use-package ethan-wspace
  :straight (:host github :repo "glasserc/ethan-wspace")
  :config
  (setq mode-require-final-newline nil)
  (global-ethan-wspace-mode 1))
