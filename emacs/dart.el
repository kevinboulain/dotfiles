(use-package dart-mode
  :ensure t
  :quelpa ((dart-mode :fetcher github :repo "bradyt/dart-mode"))
  :hook (;; flycheck isn't enabled automatically otherwise
         (dart-mode . flycheck-mode)
         ;; pub global activate dart_language_server
         (dart-mode . (lambda ()
                        (when (require 'lsp-mode nil t)
                          (lsp-define-stdio-client lsp-dart "dart"
                                                   (lambda () default-directory)
                                                   '("dart_language_server"))
                          (lsp-dart-enable))))
         ))
