(when (require 'cc-mode nil t)
  (setq c-basic-offset tab-width)
  (setq c-default-style "k&r")
  (c-set-offset 'arglist-intro '+)
  (c-set-offset 'arglist-close 0)
  (c-set-offset 'case-label '+)
  ;; (c-toggle-auto-state 1)
  ;; (c-toggle-hungry-state 1)
  )

(when (require 'clang-format nil t)
  ;; (global-set-key [C-M-tab] 'clang-format-region)
  )

;; TODO: check for {company,flycheck}-rtags
(use-package rtags
  :ensure t
  :quelpa ((rtags :fetcher github :repo "Andersbakken/rtags")))

(use-package cmake-ide
  :ensure t
  :quelpa ((cmake-ide :fetcher github :repo "atilaneves/cmake-ide"))
  :config
  (setq cmake-ide-build-pool-use-persistent-naming t)
  (cmake-ide-setup))
