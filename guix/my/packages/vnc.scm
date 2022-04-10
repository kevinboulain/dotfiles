(define-module (my packages vnc)
  #:use-module (guix build-system meson)
  #:use-module (guix git-download)
  #:use-module ((guix licenses) #:select (isc))
  #:use-module (guix packages)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages freedesktop)
  #:use-module (gnu packages image)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages xdisorg))

(define-public aml
  (package
   (name "aml")
   (version "v0.2.1")
   (source
    (origin
     (method git-fetch)
     (uri (git-reference
           (url "https://github.com/any1/aml")
           (commit version)))
      (file-name (git-file-name name version))
      (sha256 (base32 "1m911n3rd41ch4yk3k9k1lz29xp3h54k6jx122abq5kmngy9znqw"))))
   (build-system meson-build-system)
   (synopsis "Andri's Main Loop.")
   (description "An event loop.")
   (home-page "https://github.com/any1/aml")
   (license isc)))

(define-public neatvnc
  (package
   (name "neatvnc")
   (version "v0.4.0")
   (source
    (origin
     (method git-fetch)
     (uri (git-reference
           (url "https://github.com/any1/neatvnc")
           (commit version)))
     (file-name (git-file-name name version))
     (sha256 (base32 "1wpq1vyjqra877vwc3n4i0c1dyhmabyn993cslf1k142ikyc0a8w"))))
   (build-system meson-build-system)
   (native-inputs (list pkg-config))
   (inputs (list aml libdrm pixman))
   (propagated-inputs (list gnutls libjpeg-turbo zlib))
   (synopsis "Neat VNC.")
   (description "A liberally licensed VNC server library that's intended to be fast and neat.")
   (home-page "https://github.com/any1/neatvnc")
   (license isc)))

(define-public wayvnc
  (package
   (name "wayvnc")
   (version "v0.4.1")
   (source
    (origin
     (method git-fetch)
     (uri (git-reference
           (url "https://github.com/any1/wayvnc")
           (commit version)))
     (file-name (git-file-name name version))
     (sha256 (base32 "0cws9jfnmxqycmlyllvvqzw4jsbrwwk10v9gy8wifv3c61rwgdkk"))))
   (build-system meson-build-system)
   (native-inputs
    (list pkg-config))
   ;; I'm not sure how to get libgbm (from mesa?), it's optional anyway.
   (inputs (list aml libdrm libxkbcommon linux-pam neatvnc pixman wayland))
   (synopsis "VNC server for wlroots-based Wayland compositors.")
   ;; See the FAQ (https://github.com/any1/wayvnc/blob/master/FAQ.md) for
   ;; running headless:
   ;;  WLR_BACKENDS=headless WLR_LIBINPUT_NO_DEVICES=1 sway
   ;;  WAYLAND_DISPLAY=wayland-1 wayvnc 0.0.0.0 -k us-intl
   (description "It attaches to a running Wayland session, creates virtual input
devices, and exposes a single display via the RFB protocol. The Wayland session
may be a headless one, so it is also possible to run wayvnc without a physical
display attached.")
   (home-page "https://github.com/any1/wayvnc")
   (license isc)))
