(require 'color)

(defun dim-color (rgb percent)
  ;; looks like there is no color-hex-to-rgb
  (let ((r (/ (float (string-to-number (substring rgb 1 3) 16)) (float 255)))
        (g (/ (float (string-to-number (substring rgb 3 5) 16)) (float 255)))
        (b (/ (float (string-to-number (substring rgb 5 7) 16)) (float 255))))
    (apply 'color-rgb-to-hex
           (nconc (apply 'color-hsl-to-rgb
                         (apply 'color-darken-hsl (nconc (color-rgb-to-hsl r g b) `(,percent))))
                  '(2)))))

;; https://github.com/mina86/auto-dim-other-buffers.el/issues/16
(use-package auto-dim-other-buffers
  :straight (:host github :repo "mina86/auto-dim-other-buffers.el")
  :hook (after-init . auto-dim-other-buffers-mode)
  :config
  (set-face-attribute 'auto-dim-other-buffers-face nil
                      ;; :foreground (dim-color (face-attribute 'default :foreground nil t) 5)
                      :background (dim-color (face-attribute 'default :background nil t) 5)))
