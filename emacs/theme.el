;;; theme.el --- Theme setup.

;;; Commentary:

;;; The themes are marked as deferred, so you should use `load-theme`.

;;; Code:

;; http://www.bartuka.com/pages-output/personal-emacs-configuration/
(defadvice load-theme (before theme-dont-propagate activate)
  "Disable all theme effects before enabling new ones."
  (mapc #'disable-theme custom-enabled-themes))

(use-package monokai-theme
  :defer t
  :straight (:host github :repo "oneKelvinSmith/monokai-emacs"))

(use-package tao-theme
  :defer t
  :straight (tao-theme :host github :repo "11111000000/tao-theme-emacs")
  :config
  ;; override the mode-line{,-inactive} faces to match vertical-border
  (dolist (face '(mode-line mode-line-inactive))
    (set-face-attribute face nil
                        :foreground (face-attribute 'vertical-border :foreground nil t)
                        :background (face-attribute 'vertical-border :background nil t))))

(use-package zenburn-theme
  :defer t
  :straight (:host github :repo "bbatsov/zenburn-emacs")
  :init
  (setq zenburn-override-colors-alist '(("zenburn-bg" . "#1F1F1F")))) ; darker background

;;; theme.el ends here
