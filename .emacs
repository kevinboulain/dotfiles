; some modules to load
(add-to-list 'load-path "~/.emacs.d/modules/")

; helper function
(defun dirname (file)
  (file-name-directory (directory-file-name file))
)

; pattern match against files
(defun glob (path regexp)
  (directory-files path t regexp)
)

; get the last pattern match against files (useful for version ordering)
(defun glob-last (path regexp)
  ; last return something on which we must do a car...
  (car (last (glob path regexp)))
)

; get the os type, replace gnu/linux by linux
(defvar os (replace-regexp-in-string "gnu/" "" (prin1-to-string system-type)))

; lisp magic here, should learn why ` & , force evaluation of os variable
(defvar el-files
  `("ui" "general" "c" "haskell" ,(concat "os/" os))
)

; iterate over the list of file and load each one
(while el-files
  (setq el-file (concat "~/.emacs.d/" (concat (car el-files) ".el")))
  (when (file-readable-p el-file)
    (load el-file)
  )
  (setq el-files (cdr el-files))
)
