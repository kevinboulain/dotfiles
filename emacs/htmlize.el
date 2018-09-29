(use-package s :defer t) ; for s-suffix? below

(use-package htmlize
  :defer t
  :straight (:host github :repo "hniksic/emacs-htmlize")
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
                             ))
         (htmlize-before . (lambda ()
                             (require 's)
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
                                                                 (base64-encode-string (with-temp-buffer (insert-file-contents path) (buffer-string)) t)))))))))
