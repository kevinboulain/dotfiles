; previously, the following was used:
; (when (require 'whitespace nil t)
;   (setq whitespace-line-column 80) ; highlight 80+ columns, this is the default
;   (setq whitespace-style
;     '(face ; visual impact
;       trailing empty ; everything that has too much blank
;       ; lines-tail ; highlight lines with more than whitespace-line-column columns
;       ; tabs spaces ; show tabs and spaces with faces
;       ; tab-mark space-mark ; show tabs and spaces with markers
;     )
;   ) ; C-h v whitespace-style to display possibilites
;   (global-whitespace-mode t) ; activate module
; )
; however, whitespace-mode faces mess up with actual text properties (for example, in circe)
; ethan-wspace seems to aim to do better

(defconst ethan-wspace (concat user-emacs-directory "ethan-wspace/lisp"))

(when (file-readable-p ethan-wspace)
  (add-to-list 'load-path ethan-wspace)

  (when (require 'ethan-wspace nil t)
    (setq mode-require-final-newline nil)
    (global-ethan-wspace-mode 1)
  )
)
