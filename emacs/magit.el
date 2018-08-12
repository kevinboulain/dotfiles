;; magit requires let-alist but it isn't available on melpa
(add-to-list 'quelpa-melpa-recipe-stores
             '((let-alist :fetcher url
                          :url "https://git.savannah.gnu.org/cgit/emacs.git/plain/lisp/emacs-lisp/let-alist.el"
                          :version original)))

(use-package magit
  :ensure t
  :defer t
  :quelpa ((magit :fetcher github :repo "magit/magit" :files ("lisp/*.el"))))
