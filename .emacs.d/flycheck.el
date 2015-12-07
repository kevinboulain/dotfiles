; flycheck module

(defvar flycheck "~/.emacs.d/flycheck/")

; test if the submodule exists
(when (file-readable-p flycheck)
  (add-to-list 'load-path flycheck)

  ; activate module
  ; require:
  ;   let-alist or emacs 25 (bundled with it)
  ;   dash
  ;   seq (submodule added for 0.25+)
  (when (require 'flycheck nil t)
    (add-hook 'after-init-hook #'global-flycheck-mode)

    (setq flycheck-checker-error-threshold 1000)
  )
)
