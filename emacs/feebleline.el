;; see also https://github.com/11111000000/taoline, by the creator of tao-theme
(use-package feebleline
  :ensure t
  :quelpa ((feebleline :fetcher github :repo "tautologyclub/feebleline"))
  :hook
  ;; or won't be able to show notifications
  ;; should find a way to toggle it based on circe (de)activation
  (circe-mode . (lambda () (feebleline-mode 0)))
  :config
  (setq feebleline-show-git-branch t)
  (setq feebleline-show-directory t)
  (setq feebleline-show-linenum nil)
  (feebleline-mode 1))
