; mess with the garbage collector settings to make loading faster
(defconst gc-cons-threshold-backup gc-cons-threshold)
(setq gc-cons-threshold 10000000)

; get the os type, replace gnu/linux by linux
(defvar os (replace-regexp-in-string "gnu/" "" (prin1-to-string system-type)))

; lisp magic here, should learn why ` & , force evaluation of os variable
(defvar el-files
  `(; some dependencies
    "dash"
    "seq"
    ; general configuration
    "ui"
    "linum"
    "general"
    "company"
    "flycheck"
    "xcscope"
    "haskell"
    "idris"
    "rust"
    "lua"
    "c"
    ,(concat "os/" os)
  )
)

; iterate over the list of file and load each one
(while el-files
  (defconst el-file (concat "~/.emacs.d/" (concat (car el-files) ".el")))
  (when (file-readable-p el-file)
    (load el-file)
  )
  (setq el-files (cdr el-files))
)

; restore the garbage collector settings
(setq gc-cons-threshold gc-cons-threshold-backup)
