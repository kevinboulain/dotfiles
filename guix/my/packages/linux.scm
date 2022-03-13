(define-module (my packages linux)
  #:use-module (guix packages)
  #:use-module ((gnu packages linux) #:prefix linux:)
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
