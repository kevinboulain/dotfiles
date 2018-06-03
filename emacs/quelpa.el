;; automatically install quelpa without melpa (but don't self-upgrade)
(package-initialize)
(setq quelpa-update-melpa-p nil)
(unless (require 'quelpa nil t)
  (with-temp-buffer
    (url-insert-file-contents "https://raw.github.com/quelpa/quelpa/master/bootstrap.el")
    (eval-buffer)))

;; make sure use-package uses quelpa
(setq use-package-ensure-function 'quelpa)

;; use-package quelpa integration
(quelpa
 '(quelpa-use-package
   :fetcher github
   :repo "quelpa/quelpa-use-package"))
(require 'quelpa-use-package)
