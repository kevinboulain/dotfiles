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

;; use http://www.randomsample.de/profile-dotemacs.el to show where most
;; emacs-init-time is spent (that's why everything should be unrolled below)
;; see also https://github.com/dholm/benchmark-init-el for a more precise
;; but incomplete report (as it has to loaded via the package manager)

;; the whole configuration is documented in the readme.org file
(require 'org)
(org-babel-load-file (concat user-emacs-directory "readme.org"))

;; optional personal configuration
(ether--load "personal")

;; restore the garbage collector settings
(setq gc-cons-threshold gc-cons-threshold-backup)
(makunbound 'gc-cons-threshold-backup)

;;; .emacs ends here
