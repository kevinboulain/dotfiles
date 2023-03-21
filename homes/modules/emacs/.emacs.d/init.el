;; -*- lexical-binding: t -*-

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
      (let ((path-org-change (file-attribute-status-change-time (file-attributes path-org)))
            (path-el-change (file-attribute-status-change-time (file-attributes path-el))))
        (when (or (not path-el-change) ; Might not exist yet.
                  (time-less-p path-el-change path-org-change))
          (require 'ob-tangle) ; Use the embedded Org.
          (org-babel-tangle-file path-org path-el "emacs-lisp")))
      t)))

(defun my--load-org (path-base)
  "Try to load the tangled Org file PATH-BASE (without extension)."
  (when (my--org-tangle path-base)
    (my--load path-base)))

;; The whole configuration is documented in the readme.org file.
(my--load-org (expand-file-name "readme" user-emacs-directory))
;; Optional local configuration.
(my--load (expand-file-name "local" user-emacs-directory))

;; Restore the garbage collector settings.
(setq gc-cons-threshold gc-cons-threshold-backup)
(makunbound 'gc-cons-threshold-backup)
