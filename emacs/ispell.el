;; on Debian, dictionaries are available via hunspell-* packages
;; else, retrieve .aff & .dic files here:
;;  - en_*: http://wordlist.aspell.net/dicts/
;;  - fr-*: https://www.dicollecte.org/download.php?prj=fr
;;
;; http://emacs.stackexchange.com/a/21379

(defconst spellchecker "hunspell")
(defconst languages "fr-toutesvariantes,en_US")

(if (and (>= emacs-major-version 25)
         (executable-find spellchecker)
         (require 'ispell nil t))
    (progn
      (setq ispell-program-name spellchecker)
      (setq ispell-dictionary languages)
      ;; the following may crash with:
      ;;   Wrong type argument: stringp, nil
      ;; if hunspell isn't able to find dictionaries
      ;;   export LC_ALL=en_US.UTF-8
      ;;   export DICPATH=~/Library/Spelling
      ;; use hunspell -D to check hunspell's environment
      (ispell-set-spellchecker-params)
      (ispell-hunspell-add-multi-dic languages)
      ;; it significantly slows down emacs, so no prog-mode-hook for now
      (add-hook 'text-mode-hook 'flyspell-mode)
      (add-hook 'text-mode-hook 'flyspell-buffer)
      ;; (add-hook 'prog-mode-hook 'flyspell-prog-mode)
      ;; (add-hook 'text-mode-hook 'ispell-buffer)
      ;; (add-hook 'prog-mode-hook 'ispell-comments-and-strings)
      )
  (message "Could not load ispell: missing dependency"))
