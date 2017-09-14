(global-hl-line-mode) ; highlight the line containing the cursor

(when (>= emacs-major-version 24) ; nlinum requires linum which is available in emacs 24
  ; show lines numbers
  (defconst nlinum (concat user-emacs-directory "nlinum"))

  (when (file-readable-p nlinum)
    (add-to-list 'load-path nlinum)

    (when (require 'nlinum nil t)
      ; avoid the right horizontal scroll on each new digit (slow and ridiculous for long files)
      ; (add-hook 'nlinum-mode-hook
      ;   (lambda ()
      ;     (setq-local nlinum-format
      ;       (let ; fix the numbering issue that move the buffer to the right every new digit
      ;         ((w (length (number-to-string (count-lines (point-min) (point-max))))))
      ;         (concat "%" (number-to-string w) "d ")
      ;       )
      ;     )
      ;   )
      ; )

      (set-face-attribute 'nlinum-current-line nil
        :foreground (face-attribute 'linum :foreground nil t)
        :background (face-attribute 'hl-line :background nil t)
      ) ; highlight the margin with the same line highlighting
      (setq nlinum-format "%d ")
      (setq nlinum-highlight-current-line t)
      (global-nlinum-mode t)
    )
  )
)
