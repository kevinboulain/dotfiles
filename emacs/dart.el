;;; dart.el --- Dart language support.

;;; Commentary:

;;; Install the language server with:
;;;  pub global activate dart_language_server

;;; Code:

(use-package dart-mode
  :straight (:host github :repo "bradyt/dart-mode")
  :hook ((dart-mode . flycheck-mode) ; flycheck isn't enabled automatically otherwise
         (dart-mode . (lambda ()
                        (when (require 'lsp-mode nil t)
                          (lsp-define-stdio-client lsp-dart "dart"
                                                   (lambda () default-directory)
                                                   '("dart_language_server"))
                          (lsp-dart-enable))))))

;;; dart.el ends here
