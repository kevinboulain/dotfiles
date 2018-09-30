(use-package tao-yin-theme
  :straight (tao-theme :host github :repo "11111000000/tao-theme-emacs")
  :config
  ;; override the mode-line{,-inactive} faces to match vertical-border
  (dolist (face '(mode-line mode-line-inactive))
    (set-face-attribute face nil
                        :foreground (face-attribute 'vertical-border :foreground nil t)
                        :background (face-attribute 'vertical-border :background nil t))))
