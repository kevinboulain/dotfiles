;;; .emacs --- Personal configuration.

;;; Commentary:

;;; This configuration will automatically clone the git repository of each
;;; package for easier modification.
;;; Simply symlink this file and start Emacs.

;;; Code:

;; mess with the garbage collector settings to make loading faster
(defconst gc-cons-threshold-backup gc-cons-threshold)
(setq gc-cons-threshold (* 100 1024 1024))

;; set the location of the emacs directory
(setq user-emacs-directory (concat (file-name-directory (file-truename "~/.emacs")) "emacs/"))

;; redirect annoying customize stuff to another file
(setq custom-file (concat user-emacs-directory "custom.el"))

(defun ether--load (name)
  "Load file with basename NAME from `user-emacs-directory'."
  (let ((base-file (concat user-emacs-directory name)))
    (when (not (load base-file t t)) ; automatically byte-compiling those files doesn't seem worth it
      (message (format "Unable to load %s{%s}" base-file (string-join load-suffixes ","))))))

;; use http://www.randomsample.de/profile-dotemacs.el to show where
;; most emacs-init-time is spent (that's why everything is unrolled below)

;; hard dependencies
(ether--load "straight")

;; general configuration
(ether--load "general") ; setup global emacs parameters

;; themes, so they can setup faces to be used by other modules
(ether--load "theme")
(load-theme 'tao-yin t)

;; should be installed early
(ether--load "magit") ; for feebleline, which tries to require it at load time

;; other modules
(ether--load "agda")
;; (ether--load "auto-dim-other-buffers")
(ether--load "avy")
(ether--load "c")
(ether--load "circe")
(ether--load "company")
(ether--load "dart")
(ether--load "ethan-wspace")
(ether--load "feebleline")
(ether--load "flycheck")
(ether--load "gettext")
(ether--load "htmlize")
(ether--load "idris")
(ether--load "ispell")
(ether--load "ivy")
;; (ether--load "linum")
(ether--load "lsp")
(ether--load "lua")
(ether--load "markdown")
(ether--load "python")
(ether--load "rainbow-delimiters")
(ether--load "rust")
(ether--load "shell")
(ether--load "slack")
(ether--load "which-key")
(ether--load "yasnippet")

;; optional personal configuration
(ether--load "personal")

;; restore the garbage collector settings
(setq gc-cons-threshold gc-cons-threshold-backup)
(makunbound 'gc-cons-threshold-backup)

;;; .emacs ends here
