;; mess with the garbage collector settings to make loading faster
(defconst gc-cons-threshold-backup gc-cons-threshold)
(setq gc-cons-threshold 10000000)

(defconst el-files
  `(;; some dependencies
    "quelpa"
    ;; general configuration
    "general"
    ;; some stuff that should be put first
    "zenburn" ; to setup faces
    "flycheck" ; to download dash so cmake-ide doesn't crash
    ;; other modules
    "agda"
    "avy"
    "c"
    "circe"
    "company"
    "ethan-wspace"
    "gettext"
    "htmlize"
    "idris"
    "ispell"
    "linum"
    "lsp"
    "lua"
    "magit"
    "markdown"
    "python"
    "rainbow-delimiters"
    "rust"
    "shell"
    "slack"
    "which-key"
    ;; personal configuration
    "personal"
    ))

;; set the location of the emacs directory
(setq user-emacs-directory (concat (file-name-directory (file-truename "~/.emacs")) "emacs/"))

;; redirect annoying customize stuff to another file
(setq custom-file (concat user-emacs-directory "custom.el"))

;; iterate over the list of file and load each one
(dolist (el-file el-files)
  (defconst el-file (concat user-emacs-directory el-file ".el"))
  (message el-file)
  (when (file-readable-p el-file)
    (load el-file)))

;; restore the garbage collector settings
(setq gc-cons-threshold gc-cons-threshold-backup)
(makunbound 'gc-cons-threshold-backup)
