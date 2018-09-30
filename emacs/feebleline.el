(use-package dash :defer t) ; for -keep below

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
  (feebleline-mode 1))
