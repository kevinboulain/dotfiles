(define-module (my packages terminals)
  #:use-module ((gnu packages terminals) #:prefix terminals:)
  #:use-module ((guix packages)))

;; TODO: https://issues.guix.gnu.org/53257
(define-public foot
  (package
   (inherit terminals:foot)
   (native-search-paths
    ;; This isn't specified in the original foot package, leading to missing
    ;; termcap definitions. An example of where this is important is when
    ;; dealing with true colors in tmux: without the proper definitions, it
    ;; will approximate the RGB colors to the nearest 256 colors.
    ;; See https://issues.guix.gnu.org/53257.
    (list (search-path-specification
           (variable "TERMINFO_DIRS")
           (files '("share/terminfo")))))))
