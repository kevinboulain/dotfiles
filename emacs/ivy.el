;; ivy makes use of flx if it's installed
;; this provides way better matching, for example
;; M-x quer will rightfully display query-replace as the top choice
(use-package flx
  :defer t
  :straight (:host github :repo "lewang/flx"))

;; provides ivy, swiper & counsel
;; to get swiper as a replacement of isearch, ivy-yank-word
;; should probably be reassigned to C-s
(use-package ivy
  :straight (:host github :repo "abo-abo/swiper")
  :hook (after-init . ivy-mode)
  :init
  (setq ivy-re-builders-alist '((t . ivy--regex-fuzzy))) ; similar to ido-enable-flex-matching
  :config
  ;; ido style selection
  (define-key ivy-minibuffer-map (kbd "C-j") #'ivy-immediate-done)
  (define-key ivy-minibuffer-map (kbd "RET") #'ivy-alt-done))
