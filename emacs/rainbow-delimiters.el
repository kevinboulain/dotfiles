(use-package rainbow-delimiters
  :straight (:host github :repo "Fanael/rainbow-delimiters")
  :hook ((prog-mode . rainbow-delimiters-mode)
         (text-mode . rainbow-delimiters-mode)))
