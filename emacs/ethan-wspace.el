;; whitespace-mode faces mess up with actual text properties (for example, in circe)
;; ethan-wspace seems to aim to do better
(use-package ethan-wspace
  :ensure t
  :quelpa ((ethan-wspace :fetcher github :repo "glasserc/ethan-wspace" :files ("lisp/*.el")))
  :config
  (setq mode-require-final-newline nil)
  (global-ethan-wspace-mode 1))
