(define-module (my systems common)
  #:use-module ((guix gexp) #:select (file-append plain-file))
  #:use-module ((gnu bootloader) #:prefix bootloader:)
  #:use-module ((gnu bootloader grub) #:prefix grub:)
  #:use-module ((gnu packages admin) #:prefix admin:)
  #:use-module ((gnu packages base) #:prefix base:)
  #:use-module ((gnu packages bash) #:prefix bash:)
  #:use-module ((gnu packages certs) #:prefix certs:)
  #:use-module ((gnu packages cryptsetup) #:prefix cryptsetup:)
  #:use-module ((gnu packages disk) #:prefix disk:)
  #:use-module ((gnu packages guile) #:prefix guile:)
  #:use-module ((gnu packages guile-xyz) #:prefix guile-xyz:)
  #:use-module ((gnu packages less) #:prefix less:)
  #:use-module ((gnu packages linux) #:prefix linux:)
  #:use-module ((gnu packages man) #:prefix man:)
  #:use-module ((gnu packages ncurses) #:prefix ncurses:)
  #:use-module ((gnu packages text-editors) #:prefix text-editors:)
  #:use-module ((gnu packages wm) #:prefix wm:)
  #:use-module ((gnu services) #:prefix services:)
  #:use-module ((gnu services) #:select (service))
  #:use-module ((gnu services admin) #:prefix admin:)
  #:use-module ((gnu services avahi) #:prefix avahi:)
  #:use-module ((gnu services base) #:prefix base:)
  #:use-module ((gnu services dbus) #:prefix dbus:)
  #:use-module ((gnu services desktop) #:prefix desktop:)
  #:use-module ((gnu services ssh) #:prefix ssh:)
  #:use-module ((gnu services sysctl) #:prefix sysctl:)
  #:use-module ((gnu system) #:prefix system:)
  #:use-module ((gnu system nss) #:prefix nss:)
  #:use-module ((gnu system setuid) #:prefix setuid:)
  #:use-module ((nongnu packages linux) #:prefix linux:)
  #:use-module ((nongnu system linux-initrd) #:prefix linux-initrd:)
  #:use-module ((my packages bootloaders) #:prefix my:)
  #:use-module ((my services desktop) #:prefix my:)
  #:use-module ((my packages linux) #:prefix my:)
  #:export (%bootloader-configuration
            %desktop-services
            %desktop-setuid-programs
            %guix-authorized-keys
            %guix-substitute-urls
            %minimal-packages
            %minimal-services
            %operating-system
            console-services))

;; Install the basic packages on a system. @code{%base-packages} is bloated
;; (for example, why do I need at least three different editors?) and not used.
(define %minimal-packages
  (append
   (list
    admin:inetutils ; ping, hostname, ...
    admin:sudo ; Already pulled by %setuid-programs, listing it here provides manpages.
    certs:nss-certs ; Mozilla-approved certs.
    cryptsetup:cryptsetup ; The system should be encrypted.
    disk:dosfstools ; For an EFI partition.
    disk:parted ; For partitioning.
    guile-xyz:guile-colorized guile:guile-readline ; Make the Guile REPL more enjoyable.
    less:less ; Not strictly required but guix commands assume it's available.
    linux:e2fsprogs ; It's very likely the system uses Ext4.
    linux:iproute ; ip
    linux:lvm2 ; If the system uses cryptsetup, it's likely LVM sits on top.
    man:man-db ; man
    ncurses:ncurses ; Up-to-date Terminfo database (e.g.: fancy $TERM over SSH).
    text-editors:mg) ; Until Emacs gets installed by the user.
   system:%base-packages-linux ; kmod, lspci, ...
   system:%base-packages-utils)) ; ps, gzip, diff, coreutils, ...

(define %guix-authorized-keys
  (cons* (plain-file
          "nonguix.pub"
          "(public-key (ecc (curve Ed25519)
                            (q #C1FD53E5D4CE971933EC50C9F307AE2171A2D3B52C804642A7A35F84F3A4EA98#)))")
         base:%default-authorized-guix-keys))

(define %guix-substitute-urls
  (cons* "https://substitutes.nonguix.org" base:%default-substitute-urls))

(define %guix-services
  (list
   ;; Each system publishes its store and is configured to discover other
   ;; local systems' (via Avahi).
   (service base:guix-publish-service-type ; guix archive --authorize < /etc/guix/signing-key.pub
            (base:guix-publish-configuration
             (host "::")
             (advertise? #t)))
   (service base:guix-service-type
            (base:guix-configuration
             (inherit (base:guix-configuration))
             (discover? #t)
             ;; Include nonguix's nonfree substitues, see
             ;; https://gitlab.com/nonguix/nonguix.
             (authorized-keys %guix-authorized-keys)
             (substitute-urls %guix-substitute-urls)))))

(define %minimal-network-services
  (list
   (service avahi:avahi-service-type) ; Not really minimal but makes some things work out of the box...
   (service base:static-networking-service-type ; Configure the loopback interface.
            (list base:%loopback-static-networking))
   (service base:nscd-service-type)
   (service ssh:openssh-service-type
            (ssh:openssh-configuration (permit-root-login 'prohibit-password)))))

(define %minimal-system-services
  (list
   (service sysctl:sysctl-service-type)
   (service base:udev-service-type
            (base:udev-configuration
             (rules (list
                     linux:crda ; Wi-Fi regulatory domain.
                     linux:lvm2)))) ; LVM, in case the system uses it.
   (service base:urandom-seed-service-type) ; Seed the RNG.
   (service base:login-service-type ; PAM.
            (base:login-configuration
             (motd (plain-file "motd" "")))) ; Be quiet.
   ;; Logging.
   (base:syslog-service)
   (service admin:rottlog-service-type)
   ;; Copied over from %base-services: set up symlinks to keep shebangs working.
   (service services:special-files-service-type
            `(("/bin/sh" ,(file-append bash:bash "/bin/sh"))
              ("/usr/bin/env" ,(file-append base:coreutils "/bin/env"))))))

(define %minimal-services
  (append
   (list
    ;; Guix home requires XDG_RUNTIME_DIR. This would usually be set by elogind
    ;; but we try to steer clear from it and may use seatd when necessary.
    ;; See https://lists.gnu.org/archive/html/guix-devel/2022-03/msg00063.html.
    (service my:pam-dumb-runtime-dir-service-type))
   %guix-services
   %minimal-network-services
   %minimal-system-services))

;; We don't have a direct access to the sheperd-service of the following
;; services. There are three distinct types manipulated when declaring a
;; service:
;;  $x-service, a service instance
;;  $x-service-type, a service declaration (defines an extension over
;;    shepherd-root-service-type with the next one)
;;  $x-shepherd-service, a service definition
;; It would have been nice to define a record type to configure our console
;; (via @code{modify-services}) but that would require us to define a service
;; and its service type, which would then need to reference all the service
;; definitions.
(define* (console-services #:key (font #nil) (ttys #nil))
  "Set up console services. @var{font} should be the path to a font, for example
@code{(file-append font-terminus \"/share/consolefonts/ter-132n\")}. The number
of @code{ttys} can configured."
  (let ((ttys (map (lambda (tty) (format #f "tty~a" tty)) (iota (if ttys ttys 3) 1))))
    (append
     (list (service base:virtual-terminal-service-type)) ; UTF-8 terminals.
     (if font
         (list (service base:console-font-service-type ; Set the font on each of terminal.
                        (map (lambda (tty) (cons tty font)) ttys)))
         #nil)
     (map (lambda (tty) (service base:mingetty-service-type ; Start getty on each terminal.
                                 (base:mingetty-configuration (tty tty)))) ttys))))

;; Set up a nonfree kernel and initrd.
;; Also install a few basic packages and configure a few basic services
;; (including @code{%guix-service} to speed up builds by using the corresponding
;; substitutions).
(define %operating-system
  (system:operating-system
   ;; The operating-system record requires these fields to be set but they make
   ;; little sense when only providing defaults.
   (bootloader #nil)
   (file-systems #nil)
   (host-name #nil)
   (timezone #nil)
   ;; Be quiet.
   (issue "")
   ;; The bare minimum for a usable system.
   (packages %minimal-packages)
   (services (append (console-services) %minimal-services))
   (name-service-switch nss:%mdns-host-lookup-nss) ; Resolve over multicast DNS (for Avahi).
   ;; See https://gitlab.com/nonguix/nonguix.
   (kernel linux:linux) ; A kernel with blobs.
   (firmware (list my:linux-firmware)) ; All firmwares.
   (initrd linux-initrd:microcode-initrd))) ; All microcodes.

(define %bootloader-configuration
  (bootloader:bootloader-configuration
   (bootloader
    (bootloader:bootloader
     ;; Must boot in UEFI mode (grub-install does autodetection).
     (inherit grub:grub-efi-bootloader)
     ;; GRUB will embed the necessary modules for booting from the boot
     ;; partition into the EFI image. As such, it's best to have /boot shared
     ;; with /root or some tweaks to the
     ;; @code{configuration-file-generator} will have to be made (for example,
     ;; 'insmod lvm' if LVM2 sits on top of LUKS2).
     ;; However it still doesn't support booting from LUKS2 but patches exist.
     (package my:grub-efi)))
   (targets '("/boot/efi"))))

(define %desktop-services
  (list
   ;; Let's face it: the system D-Bus will be started sooner or later.
   (dbus:dbus-service)
   ;; Can replace elogind (Sway supports it).
   ;; See https://lists.gnu.org/archive/html/guix-devel/2022-03/msg00063.html.
   (service my:seatd-service-type)))

(define %desktop-setuid-programs
  ;; See https://github.com/swaywm/swaylock/issues/142.
  ;; TODO: swaylock supports PAM but Guix disables it.
  (list
   (setuid:file-like->setuid-program
    (file-append wm:swaylock "/bin/swaylock"))))
