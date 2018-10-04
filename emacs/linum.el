;;; linum.el --- Efficient line numbering.

;;; Commentary:

;;; Display line numbers in the left margin.
;;; Known to add some spaces in htmlize's output.
;;;
;;; As opposed to the builtin `linum-mode', `nlinum' doesn't freeze Emacs
;;; but may not instantaneously refresh the line numbers.

;;; Code:

(use-package nlinum
  :straight (:host github :repo "emacsmirror/nlinum")
  :config
  ;; highlight the margin with the same line highlighting
  ;; useful with `global-hl-line-mode'
  (set-face-attribute 'nlinum-current-line nil
                      :foreground (face-attribute 'linum :foreground nil t)
                      :background (face-attribute 'hl-line :background nil t))
  (setq nlinum-format "%d "
        nlinum-highlight-current-line t)
  (global-nlinum-mode 1))

;;; linum.el ends here
