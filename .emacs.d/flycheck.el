(defconst flycheck (concat user-emacs-directory "flycheck"))

(when (file-readable-p flycheck)
  (add-to-list 'load-path flycheck)

  ; requirements:
  ;   let-alist or emacs 25 (bundled with it)
  ;   dash
  ;   seq (submodule added for 0.25+)
  (if (and (>= emacs-major-version 25)
           (featurep 'dash)
           (featurep 'seq))
    (when (require 'flycheck nil t)
      (setq flycheck-checker-error-threshold nil)
      (add-hook 'prog-mode-hook #'flycheck-mode)
    )
    (message "Could not load flycheck: missing dependencies")
  )
)
