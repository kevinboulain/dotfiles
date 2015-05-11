; get the os type, replace gnu/linux by linux
(defvar os (replace-regexp-in-string "gnu/" "" (prin1-to-string system-type)))

; lisp magic here, should learn why ` & , force evaluation of os variable
(defvar el-files
  `("ui"
    "general"
    "tab"
    "c"
    "haskell"
    "idris"
    ,(concat "os/" os)
  )
)

; iterate over the list of file and load each one
(while el-files
  (setq el-file (concat "~/.emacs.d/" (concat (car el-files) ".el")))
  (when (file-readable-p el-file)
    (load el-file)
  )
  (setq el-files (cdr el-files))
)
