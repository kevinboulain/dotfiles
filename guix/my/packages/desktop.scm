(define-module (my packages desktop)
  #:use-module (guix build-system gnu)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module ((guix licenses) #:select (bsd-0))
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (gnu packages linux))

(define-public dumb-runtime-dir
  (package
   (name "dumb_runtime_dir")
   (version "v1.0.4")
   (source
    (origin
     (method git-fetch)
     (uri (git-reference
           (url "https://github.com/ifreund/dumb_runtime_dir")
           (commit version)))
     (file-name (git-file-name name version))
     (sha256
      (base32 "0nrxhvbh3bs4pi4f5h03zw1p1ys19qmmlx263ysly8302wkxk1m4"))))
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
   (inputs (list linux-pam))
   (synopsis "Creates an XDG_RUNTIME_DIR directory on login")
   (description
    "Creates an XDG_RUNTIME_DIR directory on login per the freedesktop.org base
directory spec. Flaunts the spec and never removes it, even after last logout.")
   (home-page "https://github.com/ifreund/dumb_runtime_dir")
   (license bsd-0)))
