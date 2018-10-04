;;; avy.el --- Avy (and derivatives) setup.

;;; Commentary:

;;; Jump around between words and buffers.
;;;
;;; Use C-o $letter $characters to jump to the first letter of a word.
;;; The characters are directly overlayed on the words.
;;;
;;; Use M-o [x (delete) | m (swap) | ...] $index to jump to/act on a buffer.
;;; The index appears in the top left corner.

;;; Code:

(use-package avy
  :straight (:host github :repo "abo-abo/avy")
  :config
  ;; I think I never used open-line, so override default binding
  (global-set-key (kbd "C-o") 'avy-goto-word-or-subword-1))

;; despite the name, it's avy-based
(use-package ace-window
  :straight (:host github :repo "abo-abo/ace-window")
  :config
  (setq aw-background nil) ; don't remove colors
  ;; override facemenu default binding
  (global-set-key (kbd "M-o") 'ace-window))

;;; avy.el ends here
