;;; straight.el --- Straight setup.

;;; Commentary:

;;; An alternative to `package'.
;;; Chosen over Quelpa because it doesn't keep two copies of the installed
;;; package (the source and the build) and as such, ease modifications.

;;; Code:

(setq straight-repository-branch "develop")

;; automatically install straight
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; install use-package
(straight-use-package 'use-package)

;;; straight.el ends here
