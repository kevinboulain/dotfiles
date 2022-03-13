(define-module (my packages audio)
  #:use-module (guix build-system gnu)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module ((guix licenses) #:select (lgpl2.1))
  #:use-module (guix packages)
  #:use-module (guix utils))

;; Fedora and OpenSUSE refused to package the library due to the unclear patent
;; situation. Surprisingly, Debian and Gentoo did (and Arch, also).
(define-public libfreeaptx
  (package
   (name "libfreeaptx")
   (version "0.1.1")
   (source
    (origin
     (method git-fetch)
     (uri (git-reference
           (url "https://github.com/iamthehorker/libfreeaptx")
           (commit version)))
     (file-name (git-file-name name version))
     (sha256
      (base32 "1fm5041nd08yzg0m9474g0943lb3x54zmn59b53nhvxan8x22ibq"))))
   (build-system gnu-build-system)
   (arguments
    (list
     #:make-flags
     #~(list (string-append "CC=" #$(cc-for-target))
             (string-append "LDFLAGS=-Wl,-rpath=" #$output "/lib")
             (string-append "PREFIX=" #$output))
     #:phases
     #~(modify-phases %standard-phases
        (delete 'configure)) ; No configure.
     #:tests? #f)) ; No tests.
   (synopsis "Free implementation of Audio Processing Technology codec (aptX)")
   (description
    "libfreeaptx is based on version 0.2.0 of libopenaptx with the
intent of continuing under a free license without the additional license
restriction added to 0.2.1.")
   (home-page "https://github.com/iamthehorker/libfreeaptx")
   (license lgpl2.1)))
