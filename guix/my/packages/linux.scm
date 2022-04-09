(define-module (my packages linux)
  #:use-module (guix download)
  #:use-module (guix packages)
  #:use-module ((gnu packages linux) #:prefix linux:)
  #:use-module ((nongnu packages linux) #:prefix linux:)
  #:use-module ((my packages audio) #:prefix my:))

(define-public pipewire
  (package
   (inherit linux:pipewire-0.3)
   (inputs
    (modify-inputs (package-inputs linux:pipewire-0.3)
      (prepend my:libfreeaptx)))))

(define-public wireplumber
  (package
   (inherit linux:wireplumber)
   (inputs
    (modify-inputs (package-inputs linux:wireplumber)
     ;; Otherwise it would reference the original PipeWire, without the codec.
     (replace "pipewire" pipewire)))))

(define-public linux-firmware
  (package
   (inherit linux:linux-firmware)
   (version "20220310")
   (source (origin
            (method url-fetch)
            (uri (string-append "https://git.kernel.org/pub/scm/linux/kernel"
                                "/git/firmware/linux-firmware.git/snapshot/"
                                "linux-firmware-" version ".tar.gz"))
            (sha256 (base32 "0a7jgxaz8xnvzgw754qkn45193wrlbgb3l3dgzym4jqlm1xlmhzl"))))))
