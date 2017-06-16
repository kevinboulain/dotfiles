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
