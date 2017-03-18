(when (require 'cc-mode nil t)
  (setq c-basic-offset tab-width)
  (setq c-default-style "k&r")
  (c-set-offset 'arglist-intro '+)
  (c-set-offset 'arglist-close 0)
  (c-set-offset 'case-label '+)
  ; (c-toggle-auto-state 1)
  ; (c-toggle-hungry-state 1)
)

(when (require 'clang-format nil t)
  ; from the docs
  ; (global-set-key [C-M-tab] 'clang-format-region)
)

(defconst cmake-ide (concat user-emacs-directory "cmake-ide"))

(when (file-readable-p cmake-ide)
  (add-to-list 'load-path cmake-ide)

  (if (featurep 'seq)
    (when (require 'cmake-ide nil t)
      ; may cause problems if CMakeLists.txt change
      ; (setq cmake-ide-build-pool-use-persistent-naming t)
      ; the other variables doesn't seem to work properly...
      (cmake-ide-setup)
    )
    (message "Could not load cmake-ide: missing dependencies")
  )
)
