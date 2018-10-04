;;; ispell.el --- ispell setup.

;;; Commentary:

;;; You'll need to install hunspell.
;;;
;;; On Debian, dictionaries are available via hunspell-* packages
;;; else, retrieve .aff & .dic files here:
;;;  - en_*: http://wordlist.aspell.net/dicts/
;;;  - fr-*: https://www.dicollecte.org/download.php?prj=fr
;;;
;;; It may cause significant slow down.
;;;
;;; If not run automatically, use M-x flyspell-{buffer,mode}.
;;; Use M-$ to correct an highlighted word.
;;;
;;; http://emacs.stackexchange.com/a/21379

;;; Code:

(defconst ether--spellchecker "hunspell")
(defconst ether--languages "fr-toutesvariantes,en_US")

(if (and (>= emacs-major-version 25)
         (executable-find ether--spellchecker)
         (require 'ispell nil t))
    (progn
      (setq ispell-program-name ether--spellchecker)
      (setq ispell-dictionary ether--languages)
      ;; the following may crash with:
      ;;   Wrong type argument: stringp, nil
      ;; if hunspell isn't able to find dictionaries
      ;;   export LC_ALL=en_US.UTF-8
      ;;   export DICPATH=~/Library/Spelling
      ;; use hunspell -D to check hunspell's environment
      (ispell-set-spellchecker-params)
      (ispell-hunspell-add-multi-dic ether--languages)
      ;; it significantly slows down emacs, so no prog-mode-hook for now
      ;; (add-hook 'prog-mode-hook 'flyspell-prog-mode)
      ;; (add-hook 'text-mode-hook 'ispell-buffer)
      ;; (add-hook 'prog-mode-hook 'ispell-comments-and-strings)
      ;; (add-hook 'text-mode-hook 'flyspell-buffer)
      (add-hook 'text-mode-hook 'flyspell-mode))
  (message "Unable to load ispell due to missing dependency"))

;;; ispell.el ends here
