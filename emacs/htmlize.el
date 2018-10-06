;;; htmlize.el --- Render an HTML version of a buffer.

;;; Commentary:

;;; Useful for sharing properly rendered code.
;;;
;;; Use M-x htmlize-buffer.
;;;
;;; Or C-SPC two times then move the point to the end of the region then
;;; M-x htmlize-region (so you don't render the region selection overlay).
;;;
;;; Or even emacs --htmlize path/to/file which will output path/to/file.html.

;;; Code:

(use-package s :defer t) ; for s-suffix?

(defun ether--htmlize-file (switch)
  "`htmlize-file' seems to have some issues, here is a simpler one."
  (ignore switch)
  (condition-case out
      (progn
        (require 'htmlize)
        (let* ((source (pop command-line-args-left))
               (destination (htmlize-make-file-name (file-name-nondirectory source))))
          (find-file-existing source)
          (with-current-buffer (htmlize-buffer-1)
            (write-region (point-min) (point-max) destination))
          (kill-emacs 0)))
    (error (progn
             (princ out) ; looks like we can't really use external-debugging-output
             (kill-emacs 1)))))

(use-package htmlize
  :defer t
  :straight (:host github :repo "hniksic/emacs-htmlize")
  :init
  (add-to-list 'command-switch-alist '("--htmlize" . ether--htmlize-file))
  :config
  ;; use the Iosevka font if available (ligatures may be nice for displayed code)
  ;; requires fontconfig, works on Linux and macOS
  ;; use %{=unparse} format to see all options
  ;; regular Iosevka instead of Iosevka Term seems to produce some irregularities
  ;; for example let-alist and use-package seems to be slightly larger
  (let* ((fc-match "fc-match -f '%{file}' 'Iosevka Term:style=Regular'")
         (path (shell-command-to-string fc-match)))
    (when (s-suffix? ".ttf" path :ignore-case) ; Firefox doesn't like .ttc
      (setq htmlize-head-tags (format "    <style type=\"text/css\">
      @font-face {
        font-family: Iosevka;
        src: url(data:font/ttf;base64,%s) format('truetype');
      }
      pre {
        font-family: Iosevka;
        font-size: 10pt;
      }
    </style>
"
                                      (base64-encode-string (with-temp-buffer (insert-file-contents path) (buffer-string)) t)))))
  :hook ((htmlize-before . (lambda ()
                             ;; disable some modes that may influence rendering
                             ;; the original buffer is protected, no need to restore
                             ;; flycheck handling (use special faces that are not nicely rendered)
                             (when (bound-and-true-p flycheck-mode) (flycheck-mode))
                             ;; disable current line highlighting (both a function and a variable)
                             (when (bound-and-true-p global-hl-line-mode) (global-hl-line-unhighlight))
                             ;; disable line numbering (the format adds a an empty column)
                             ;; don't know why, but simply toggling without the 0 won't work
                             (when (bound-and-true-p nlinum-mode) (nlinum-mode 0))
                             ;; TODO: lsp stuff
                             ;; (sit-for 3) ; allow to see the changes made to the buffer
                             ))))
;;; htmlize.el ends here
