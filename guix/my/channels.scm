;; It's not necessary to define a module but it makes things easier when
;; loading paths (otherwise Guile would complain 'channel' isn't defined).
(define-module (my channels)
  #:use-module (guix channels)
  #:export (%channels))

(define %channels
  (list
   (channel
    (name 'nonguix)
    (url "https://gitlab.com/nonguix/nonguix")
    (commit "fea52adbc9356184bff51146c6515fad609baf77")
    (introduction
     (make-channel-introduction
      "897c1a470da759236cc11798f4e0a5f7d4d59fbc"
      (openpgp-fingerprint "2A39 3FFF 68F4 EF7A 3D29  12AF 6F51 20A0 22FB B2D5"))))
   (channel
    (name 'guix)
    (url "https://git.savannah.gnu.org/git/guix.git")
    (commit "d41c82b481fd0f5c7d45d6e2629fdf9d2085205b")
    (introduction
     (make-channel-introduction
      "9edb3f66fd807b096b48283debdcddccfea34bad"
      (openpgp-fingerprint "BBB0 2DDF 2CEA F6A8 0D1D  E643 A2A0 6DF2 A33A 54FA"))))))

%channels
