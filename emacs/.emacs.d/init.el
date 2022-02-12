;;; .emacs --- Personal configuration. -*- lexical-binding: t -*-

;;; Commentary:

;;; This configuration will automatically clone the git repository of each
;;; package for easier modification.
;;; Simply symlink this file and start Emacs.

;;; Code:

;; Mess with the garbage collector settings to make loading faster.
(defconst gc-cons-threshold-backup gc-cons-threshold)
(setq gc-cons-threshold (* 100 1024 1024))

(defun ether--find-loaded-user-init-file ()
  "Find out if this file was loaded with -l or --load and return its path."
  (require 'cl-macs)
  (let (last)
    (cl-dolist (argument command-line-args)
      (when (and (member last '("-l" "--load"))
                 (string= argument load-file-name))
        (cl-return argument))
      (setq last argument))))

(defvar ether--emacs-directory
   (file-name-directory  ;; emacs/
    (directory-file-name (file-name-directory ;; emacs/.emacs
                          (file-truename ;; emacs/.emacs.d/init.el
                           ;; `user-init-file' isn't set when -q is set.
                           ;; Don't forget `after-init-hook' is done
                           ;; before -l so you might have to call the
                           ;; functions yourself.
                           (if user-init-file
                               user-init-file
                             (ether--find-loaded-user-init-file)))))))

;; Redirect `customize' stuff to another file.
(setq custom-file (locate-user-emacs-file "custom.el"))
(load custom-file t)

(defun ether--load-warn (path-base)
  "Try to load PATH-BASE (with or without extension) or warn about it."
  (require 'subr-x)
  (when (not (load path-base t t)) ; Automatically byte-compiling those files doesn't seem worth it.
    (message (format "Unable to load %s{%s}" path-base (string-join load-suffixes ",")))))

(defun ether--load (name)
  "Load file with basename NAME from `ether--emacs-directory'.
Instead of relying on `org-babel-load-file' (which may overwrite lentic's),
reimplement a safer logic here, for the details, see:
https://github.com/phillord/lentic/issues/54#issuecomment-429106163"
  (let* ((path-org (concat ether--emacs-directory name ".org"))
         (temporary-path-org (concat temporary-file-directory name ".el"))
         (path-base (concat ether--emacs-directory name)))
    (if (file-readable-p path-org) ; Fallbacking may load the lentic file...
        (progn
          (when (file-newer-than-file-p path-org temporary-path-org)
            (require 'org) ; Use the embedded Org.
            (org-babel-tangle-file path-org temporary-path-org "emacs-lisp"))
          (ether--load-warn temporary-path-org))
      (ether--load-warn path-base))))

;; The whole configuration is documented in the readme.org file.
(ether--load "readme")
;; Optional local configuration.
(ether--load "local")

;; Restore the garbage collector settings.
(setq gc-cons-threshold gc-cons-threshold-backup)
(makunbound 'gc-cons-threshold-backup)

;;; .emacs ends here
