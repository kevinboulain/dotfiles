; http://extensions.libreoffice.org/extension-center/dictionnaires-francais
; http://extensions.libreoffice.org/extension-center/english-dictionaries
; http://emacs.stackexchange.com/a/21379

(defconst spellchecker "hunspell")

(when (>= emacs-major-version 25)
  (if (executable-find spellchecker)
    (progn
      (with-eval-after-load "ispell"
        (setq ispell-program-name spellchecker)
        (setq ispell-dictionary "fr_FR,en_GB")
        ; the following may crash with:
        ;   Wrong type argument: stringp, nil
        ; if hunspell isn't able to find dictionaries
        ;   export LC_ALL=en_US.UTF-8
        ;   export DICPATH=~/Library/Spelling
        ; use hunspell -D to check hunspell's environment
        (ispell-set-spellchecker-params)
        (ispell-hunspell-add-multi-dic "fr_FR,en_GB")
      )
      (add-hook 'text-mode-hook 'flyspell-mode)
      (add-hook 'text-mode-hook 'flyspell-buffer)
      (add-hook 'prog-mode-hook 'flyspell-prog-mode)
      ; (add-hook 'text-mode-hook 'ispell-buffer)
      ; (add-hook 'prog-mode-hook 'ispell-comments-and-strings)
    )
    (message "Could not load ispell: missing program")
  )
)
