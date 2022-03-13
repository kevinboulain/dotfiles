;; guix home --dry-run --load-path="$(pwd)" reconfigure my/home/desktop.scm

(define-module (my home desktop)
  #:use-module (guix gexp)
  #:use-module (gnu home)
  #:use-module (gnu home services)
  #:use-module (gnu home services shepherd)
  #:use-module (gnu packages fonts)
  #:use-module (gnu packages fontutils)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages pulseaudio)
  #:use-module (gnu packages terminals)
  #:use-module (gnu packages wm)
  #:use-module (gnu services)
  #:use-module ((my packages linux) #:prefix my:)
  #:use-module ((my packages terminals) #:prefix my:)
  #:use-module ((my packages vnc) #:prefix my:)
  #:use-module ((my home minimal) #:prefix my:)
  #:use-module (nongnu packages mozilla))

(define (configure-fontconfig-service presets)
  "Enable Fontconfig presets. @var{presets} a list of presets (from Fontconfig's
@file{conf.avail})."
  (simple-service
   'configure-fontconfig
   ;; @code{home-files-service-type} expects a list of files.
   ;; One usually needs to write their contents to the store (via a gexp) and
   ;; return their parths for them to be symlinked. Since all we want to do
   ;; here is symlink files from another package, we only need to provide a
   ;; mapping from home directory files to package files.
   home-files-service-type
   (map (lambda (preset)
          (list
           (string-append "config/fontconfig/conf.d/" preset)
           (file-append fontconfig "/share/fontconfig/conf.avail/" preset)))
        presets)))

(define (dbus-environment-variables-service _)
  `(("DBUS_SESSION_BUS_ADDRESS" . "unix:path=$XDG_RUNTIME_DIR/dbus")))

(define (dbus-shepherd-service _)
  (list
   (shepherd-service
    (documentation "Run D-Bus.")
    (provision '(dbus))
    (start
     #~(make-forkexec-constructor
        (list #$(file-append dbus "/bin/dbus-daemon")
              #$(string-append "--address=unix:path=" (getenv "XDG_RUNTIME_DIR") "/dbus")
              "--nofork"
              "--session")
        #:log-file (string-append (getenv "XDG_LOG_HOME") "/dbus.log")))
    (stop #~(make-kill-destructor)))))

(define dbus-service-type
  (service-type
   (name 'dbus)
   (extensions
    (list
     (service-extension home-environment-variables-service-type dbus-environment-variables-service)
     (service-extension home-shepherd-service-type dbus-shepherd-service)))
   (default-value #nil)
   (description "Run D-Bus.")))

(define (pipewire-shepherd-service _)
  (list
   (shepherd-service
    (documentation "Run PipeWire.")
    (provision '(pipewire))
    (requirement '(dbus))
    (start
     #~(make-forkexec-constructor
        (list #$(file-append my:pipewire "/bin/pipewire"))
        #:environment-variables (append (list "DISABLE_RTKIT=1")
                                        (default-environment-variables))
        #:log-file (string-append (getenv "XDG_LOG_HOME") "/pipewire.log")))
    (stop #~(make-kill-destructor)))
   (shepherd-service
    (documentation "Run the PipeWire PulseAudio shim.")
    (provision '(pipewire-pulse))
    (requirement '(dbus pipewire))
    (start
     #~(make-forkexec-constructor
        (list #$(file-append my:pipewire "/bin/pipewire-pulse"))
        #:environment-variables (append (list "DISABLE_RTKIT=1")
                                        (default-environment-variables))
        #:log-file (string-append (getenv "XDG_LOG_HOME") "/pipewire-pulse.log")))
    (stop #~(make-kill-destructor)))
   (shepherd-service
    (documentation "Run WirePlumber. Necessary to connect streams to outputs.")
    (provision '(wireplumber))
    (requirement '(dbus pipewire))
    (start
     #~(make-forkexec-constructor
        (list #$(file-append my:wireplumber "/bin/wireplumber"))
        #:environment-variables (append (list "DISABLE_RTKIT=1")
                                        (default-environment-variables))
        #:log-file (string-append (getenv "XDG_LOG_HOME") "/wireplumber.log")))
    (stop #~(make-kill-destructor)))))

(define pipewire-service-type
  (service-type
   (name 'pipewire)
   (extensions
    (list
     (service-extension home-shepherd-service-type pipewire-shepherd-service)))
   (default-value #nil)
   (description "Run PipeWire.")))

(home-environment
 (packages
  (cons*
   ;; Wayland.
   sway
   my:wayvnc
   firefox/wayland
   ;; wm:waybar
   font-iosevka-term
   fzf ; For Sway, as a replacement of dmenu & co.
   mako ; For Sway, notification daemon.
   my:foot
   ;; Audio.
   my:pipewire ; For pw-*.
   pavucontrol ; Uses @code{pipewire-pulse}.
   ;; The rest.
   my:%minimal-packages))
 (services
  (list
   (configure-fontconfig-service
    ;; From my limited understanding and tests, these give the best results
    ;; (no rainbowy subpixel rendering like 10-sub-pixel-rgb.conf would
    ;; introduce).
    ;; See https://wiki.archlinux.org/title/font_configuration.
    '("10-hinting-none.conf" "10-no-sub-pixel.conf" "70-no-bitmaps.conf"))
   (service dbus-service-type)
   (service pipewire-service-type))))
