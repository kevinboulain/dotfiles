;; mess with the garbage collector settings to make loading faster
(defconst gc-cons-threshold-backup gc-cons-threshold)
(setq gc-cons-threshold (* 100 1024 1024))

;; set the location of the emacs directory
(setq user-emacs-directory (concat (file-name-directory (file-truename "~/.emacs")) "emacs/"))

;; redirect annoying customize stuff to another file
(setq custom-file (concat user-emacs-directory "custom.el"))

(defun l (name)
  (let ((base-file (concat user-emacs-directory name)))
    (when (not (load base-file t t)) ; automatically byte-compiling those files doesn't seem worth it
      (message (format "Unable to load %s{%s}" base-file (string-join load-suffixes ","))))))

;; use http://www.randomsample.de/profile-dotemacs.el to show where
;; most emacs-init-time is spent (that's why everything is unrolled below)

;; hard dependencies
(l "straight")

;; general configuration
(l "general")

;; should be installed early
(l "tao") ; like themes, so they can setup faces to be used by other modules
;; (l "zenburn")
(l "magit") ; for feebleline, which tries to require it at load time

;; other modules
(l "agda")
(l "avy")
(l "c")
(l "circe")
(l "company")
(l "dart")
(l "ethan-wspace")
(l "feebleline")
(l "flycheck")
(l "gettext")
(l "htmlize")
(l "idris")
(l "ispell")
(l "linum")
(l "lsp")
(l "lua")
(l "markdown")
(l "python")
(l "rainbow-delimiters")
(l "rust")
(l "shell")
(l "slack")
(l "which-key")
(l "yasnippet")

;; optional personal configuration
(l "personal")

(makunbound 'l)

;; restore the garbage collector settings
(setq gc-cons-threshold gc-cons-threshold-backup)
(makunbound 'gc-cons-threshold-backup)
