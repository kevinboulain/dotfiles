;;; feebleline.el --- Unintrusive mode-line.

;;; Commentary:

;;; Completely disable the standard mode-line and show a minimalistic
;;; replacement in the minibuffer when it's idle.

;;; Code:

(use-package dash :defer t) ; for -keep

;; see also https://github.com/11111000000/taoline, by the creator of tao-theme
(use-package feebleline
  :straight (:host github :repo "tautologyclub/feebleline")
  :config
  ;; mainly from feebleline.el, but more minimalistic
  ;; note there is a timer, so not everything will be instantaneously refreshed
  (setq feebleline-mode-line-text
        '(("%s" ((let ((indicator (string-join
                                   (-keep (lambda (pair) (when (car pair) (cdr pair)))
                                          (list (cons (bound-and-true-p line-number-mode) (format-mode-line "%l"))
                                                (cons (bound-and-true-p column-number-mode) (number-to-string (current-column)))))
                                   ",")))
                   (if (> (length indicator) 0) (concat indicator " ") "")))
           (face feebleline-linum-face))
          ("%s" ((if (buffer-file-name)
                     (replace-regexp-in-string
                      feebleline--home-dir "~"
                      (file-name-directory (buffer-file-name)))
                   ""))
           (face feebleline-dir-face))
          ("%s" ((if (buffer-file-name)
                     (file-name-nondirectory (buffer-file-name))
                   (buffer-name)))
           (face feebleline-bufname-face))
          ("%s" ((if (and (buffer-file-name) (buffer-modified-p))
                     "*"
                   ""))
           (face feebleline-asterisk-face))
          ("%s" ((if (fboundp 'magit-get-current-branch) ; set by magit
                     (let ((branch (magit-get-current-branch)))
                       (if (> (length branch) 0) (concat ":" branch) ""))
                   ""))
           (face feebleline-git-branch-face))
          ("%s" ; for circe, display the list of active buffers
           ((if (boundp 'tracking-buffers) ; set by circe
                (concat " " (string-join
                             (mapcar (lambda (buffer)
                                       (let ((propertized-buffer (propertize buffer 'face 'feebleline-previous-buffer-face))) ; change the face
                                         (if (eq (get-text-property 0 'face buffer) 'circe-highlight-nick-face) ; when highlighted
                                             (concat propertized-buffer (propertize "*" 'face 'feebleline-asterisk-face)) ; append a feebleline asterisk
                                           propertized-buffer)))
                                     tracking-buffers) ; for each buffer
                             " "))
              "")))))
  ;; enable feebleline
  (feebleline-mode 1)
  ;; but still allow mode line in some cases for readability
  ;; https://emacs.stackexchange.com/questions/30513/making-a-variable-window-local
  ;; https://github.com/tautologyclub/feebleline/issues/24
  (add-hook 'window-configuration-change-hook
            (lambda ()
              (when (bound-and-true-p feebleline-mode)
                (walk-windows (lambda (window) ; reset the mode line of each buffer
                                (with-current-buffer (window-buffer window)
                                  (setq mode-line-format nil)))
                              0) ; don't include minibuffer
                (walk-windows (lambda (window) ; and set the mode line when necessary
                                (with-current-buffer (window-buffer window)
                                  ;; {frame,window}-edges functions return (x1 y1 x2 y2)
                                  ;; then check if the current buffer (which may be displayed on mutltiple windows)
                                  ;; doesn't sit on top of the minibuffer
                                  ;; this isn't perfect but unfortunately, the mode-line-format is buffer-local
                                  (when (/= (nth 1 (window-edges (minibuffer-window)))
                                            (nth 3 (window-edges window)))
                                    (setq mode-line-format "%-"))))
                              0))))) ; don't include minibuffer

;;; feebleline.el ends here
