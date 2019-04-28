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
(setq custom-file (locate-user-emacs-file "custom.el"))

(defun ether--load-warn (path-base)
  "Try to load PATH-BASE (with or without extension) or warn about it."
  (require 'subr-x)
  (when (not (load path-base t t)) ; automatically byte-compiling those files doesn't seem worth it
    (message (format "Unable to load %s{%s}" path-base (string-join load-suffixes ",")))))

(defun ether--load (name)
  "Load file with basename NAME from `user-emacs-directory'.
Instead of relying on `org-babel-load-file' (which may overwrite lentic),
reimplement a safer logic here, for the details, see:
https://github.com/phillord/lentic/issues/54#issuecomment-429106163"
  (let* ((filename-org (concat name ".org"))
         (path-org (locate-user-emacs-file filename-org))
         (temporary-path-org (concat temporary-file-directory name ".el"))
         (path-base (locate-user-emacs-file name)))
    (if (file-readable-p path-org) ; fallbacking may load the lentic file...
        (progn
          (when (file-newer-than-file-p path-org temporary-path-org)
            (require 'org) ; use the embedded Org
            (org-babel-tangle-file path-org temporary-path-org "emacs-lisp"))
          (ether--load-warn temporary-path-org))
      (ether--load-warn path-base))))

;; use http://www.randomsample.de/profile-dotemacs.el to show where most
;; emacs-init-time is spent (that's why everything should be unrolled below)
;; see also https://github.com/dholm/benchmark-init-el for a more precise
;; but incomplete report (as it has to loaded via the package manager)

;; the whole configuration is documented in the readme.org file
(ether--load "readme")
;; optional personal configuration
(ether--load "personal")

;; restore the garbage collector settings
(setq gc-cons-threshold gc-cons-threshold-backup)
(makunbound 'gc-cons-threshold-backup)

;;; .emacs ends here
