; mess with the garbage collector settings to make loading faster
(defconst gc-cons-threshold-backup gc-cons-threshold)
(setq gc-cons-threshold 10000000)

; get the os type, replace gnu/linux by linux
(defconst os (replace-regexp-in-string "gnu/" "" (prin1-to-string system-type)))

; lisp magic here, should learn why ` & , force evaluation of os variable
(defconst el-files
  `(; some dependencies
    "dash"
    "seq"
    "s"
    "levenshtein" ; https://www.emacswiki.org/emacs/levenshtein.el
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
    "spellcheck"
    "htmlize"
    "shell"
    ,(concat "os/" os)
    "personal"
  )
)

; set the location of the .emacs.d directory
(setq user-emacs-directory (concat (file-name-directory (file-truename "~/.emacs")) ".emacs.d/"))

; iterate over the list of file and load each one
(dolist (el-file el-files)
  (defconst el-file (concat user-emacs-directory el-file ".el"))
  (when (file-readable-p el-file)
    (load el-file)
  )
)

; restore the garbage collector settings
(setq gc-cons-threshold gc-cons-threshold-backup)
