;;; .emacs --- Personal configuration. -*- lexical-binding: t -*-

;;; Commentary:

;;; This configuration will automatically clone the git repository of each
;;; package for easier modification.
;;; Simply symlink this file and start Emacs.

;;; Code:

;; Mess with the garbage collector settings to make loading faster.
(defconst gc-cons-threshold-backup gc-cons-threshold)
(setq gc-cons-threshold (* 100 1024 1024))

(defun my--find-loaded-user-init-file ()
  "Find out if this file was loaded with -l or --load and return its path."
  (require 'cl-macs)
  (let (last)
    (cl-dolist (argument command-line-args)
      (when (and (member last '("-l" "--load"))
                 (string= (file-truename argument) (file-truename load-file-name)))
        (cl-return argument))
      (setq last argument))))

(defvar my--emacs-directory
   (file-name-directory  ;; emacs/
    (directory-file-name
     (file-name-directory ;; emacs/.emacs
      (file-truename ;; emacs/.emacs.d/init.el
       ;; `user-init-file' isn't set when -q is set.
       ;; Don't forget `after-init-hook' is done before -l so you might have to
       ;; call the functions yourself:
       ;;  emacs --eval '(setq user-emacs-directory ".emacs.d")' -Q -l .emacs.d/init.el
       (if user-init-file
           user-init-file
         (my--find-loaded-user-init-file)))))))

(defun my--load (path-base)
  "Try to load PATH-BASE (with or without extension) or warn about it."
  (require 'subr-x)
  (when (not (load path-base t t)) ; Automatically byte-compiling those files doesn't seem worth it.
    (message (format "Unable to load %s{%s}" path-base (string-join load-suffixes ",")))))

;; Redirect `customize' stuff to another file.
(setq custom-file (locate-user-emacs-file "custom.el"))
(my--load (file-name-sans-extension custom-file))

(defun my--org-tangle (path-base)
  "Tangle the Org file PATH-BASE (without extension) or warn about it.
When newer, stored alongside its source."
  (let* ((path-org (concat path-base ".org"))
         (path-el (concat path-base ".el")))
    (if (not (file-readable-p path-org))
        (progn
          (message "Unable to tangle %s" path-org)
          nil)
      (when (file-newer-than-file-p path-org path-el)
        (require 'ob-tangle) ; Use the embedded Org.
        (org-babel-tangle-file path-org path-el "emacs-lisp"))
      t)))

(defun my--load-org (path-base)
  "Try to load the tangled Org file PATH-BASE (without extension)."
  (when (my--org-tangle path-base)
    (my--load path-base)))

;; The whole configuration is documented in the readme.org file.
(my--load-org (expand-file-name "readme" my--emacs-directory))
;; Optional local configuration.
(my--load (expand-file-name "local" my--emacs-directory))

;; Restore the garbage collector settings.
(setq gc-cons-threshold gc-cons-threshold-backup)
(makunbound 'gc-cons-threshold-backup)

;;; .emacs ends here
