(use-package htmlize
  :ensure t
  :quelpa ((htmlize :fetcher github :repo "hniksic/emacs-htmlize"))
  :hook ((htmlize-before . (lambda ()
                             ;; disable some modes that may influence rendering
                             ;; the original buffer is protected, no need to restore
                             ;; flycheck handling (use special faces that are not nicely rendered)
                             (when (bound-and-true-p flycheck-mode) (flycheck-mode))
                             ;; disable current line highlighting (both a function and a variable)
                             (when (bound-and-true-p global-hl-line-mode) (global-hl-line-mode -1))
                             ;; disable line numbering (the format adds a an empty column)
                             (when (bound-and-true-p nlinum-mode) (global-nlinum-mode 0))
                             ;; (sit-for 3) ; allow to see the changes made to the buffer
                             ))
         (htmlize-before . (lambda ()
                             ;; use the Iosevka font if available (ligatures may be nice for displayed code)
                             (setq htmlize-head-tags
                                   ;; requires fontconfig, works on Linux and macOS
                                   ;; use %{=unparse} format to see all options
                                   (let* ((fc-match "fc-match -f '%{file}' 'Iosevka:style=Regular'")
                                          (path (shell-command-to-string fc-match)))
                                     (when (length path)
                                       (format "    <style type=\"text/css\">
      @font-face {
        font-family: Iosevka;
        src: url(data:font/ttf;base64,%s) format('truetype');
      }
      pre {
        font-family: Iosevka;
        font-size: 12px;
      }
    </style>
"
                                               (base64-encode-string (with-temp-buffer (insert-file-contents path) (buffer-string)) t)))))))))
