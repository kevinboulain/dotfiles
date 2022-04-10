((scheme-mode . ((eval . (let ((directory (locate-dominating-file default-directory ".dir-locals.el")))
                           ;; So Geiser/Guile know where to find the modules.
                           (add-to-list 'geiser-guile-load-path directory)
                           ;; Only 'guix repl' has the correct %load-path, Guile
                           ;; wouldn't know about channels. Unfortunately,
                           ;; Geiser expects an executable name.
                           (setq geiser-guile-binary (concat directory "guix-repl.sh"))))
                 ;; Not strictly necessary since it will be silently ignored on
                 ;; failure, but see https://gitlab.com/jaor/geiser/issues/298.
                 (geiser-repl-skip-version-check-p . t))))
