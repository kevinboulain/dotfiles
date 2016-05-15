; flycheck module

(defconst flycheck "~/.emacs.d/flycheck/")

; test if the submodule exists
(when (file-readable-p flycheck)
  (add-to-list 'load-path flycheck)

  ; activate module
  ; require:
  ;   let-alist or emacs 25 (bundled with it)
  ;   dash
  ;   seq (submodule added for 0.25+)
  (if (and (>= emacs-major-version 25)
           (featurep 'dash)
           (featurep 'seq))
    (when (require 'flycheck nil t)
      (add-hook 'after-init-hook #'global-flycheck-mode)

      (setq flycheck-checker-error-threshold 1000)
    )
    (message "Could not load flycheck: missing dependencies")
  )
)
