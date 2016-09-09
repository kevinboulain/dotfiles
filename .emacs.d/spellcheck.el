; http://extensions.libreoffice.org/extension-center/dictionnaires-francais
; http://extensions.libreoffice.org/extension-center/english-dictionaries
; http://emacs.stackexchange.com/a/21379

(when (>= emacs-major-version 25)
  (with-eval-after-load "ispell"
    (setq ispell-program-name "hunspell")
    (setq ispell-dictionary "fr-moderne,en_GB")
    ; the following may crash with:
    ; Wrong type argument: stringp, nil
    ; if hunspell isn't able to find a default dictionary based on the locale
    ; fix the locale so that hunspell can find a default dictionary:
    ; export LC_ALL="en_US.utf8"
    ; use hunspell -D to check hunspell's environment
    (ispell-set-spellchecker-params)
    (ispell-hunspell-add-multi-dic "fr-moderne,en_GB")
  )
  (add-hook 'text-mode-hook 'flyspell-buffer)
)
