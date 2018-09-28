;; http://www.randomsample.de/profile-dotemacs.el is nice
;; but requires to unroll the loop below

;; mess with the garbage collector settings to make loading faster
(defconst gc-cons-threshold-backup gc-cons-threshold)
(setq gc-cons-threshold (* 100 1024 1024))

(defconst el-files
  `(;; some dependencies
    "straight"
    ;; general configuration
    "general"
    ;; some stuff that should be put first
    ;; like themes, so they can setup faces early to be used by other modules
    ;; "zenburn"
    "tao"
    ;; other modules
    "agda"
    "avy"
    "c"
    "circe"
    "company"
    "dart"
    "ethan-wspace"
    "feebleline"
    "flycheck"
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
    "yasnippet"
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
  (if (file-readable-p el-file)
      (load el-file)
    (message (format "Unable to load file: %s" el-file))))

;; restore the garbage collector settings
(setq gc-cons-threshold gc-cons-threshold-backup)
(makunbound 'gc-cons-threshold-backup)
