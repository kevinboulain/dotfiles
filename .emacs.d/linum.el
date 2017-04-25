; emacs 24 specific stuff
(when (>= emacs-major-version 24)
  ; show lines numbers, see http://www.emacswiki.org/LineNumbers
  (when (require 'linum nil t)
    (add-hook 'linum-before-numbering-hook
      (lambda () ; make line counting more efficient
        (setq-local linum-format-fmt
          (let ; fix the numbering issue that move the buffer to the right every new digit
            ((w (length (number-to-string (count-lines (point-min) (point-max))))))
            (concat "%" (number-to-string w) "d")
          )
        )
      )
    )
    (setq linum-format
      (lambda (line)
        (concat
          (propertize (format linum-format-fmt line) 'face 'linum)
          (propertize " " 'face 'linum)
        )
      )
    )
    ; (setq linum-format "%d ") ; add a blank space after the line number
    (global-linum-mode 1)
  )

  ; wrapping mode for linum, highlight the cursor's line number
  (defconst hlinum "~/.emacs.d/hlinum/")

  ; test if the submodule exists
  (when (file-readable-p hlinum)
    (add-to-list 'load-path hlinum)

    (when (require 'hlinum nil t)
      (hlinum-activate)
    )
  )
)

(global-hl-line-mode) ; highlight the line containing the cursor
