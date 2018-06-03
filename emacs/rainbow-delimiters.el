(use-package rainbow-delimiters
  :ensure t
  :quelpa ((rainbow-delimiters :fetcher github :repo "Fanael/rainbow-delimiters"))
  :hook ((prog-mode . rainbow-delimiters-mode)
         (text-mode . rainbow-delimiters-mode)))
