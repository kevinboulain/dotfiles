;; guix system --dry-run --load-path="$(pwd)" reconfigure my/systems/cocytus.scm

(define-module (my systems cocytus)
  #:use-module (guix gexp)
  #:use-module (gnu bootloader)
  #:use-module (gnu bootloader grub)
  #:use-module (gnu packages fonts)
  #:use-module (gnu services)
  #:use-module (gnu services desktop)
  #:use-module (gnu services networking)
  #:use-module (gnu system)
  #:use-module (gnu system file-systems)
  #:use-module (gnu system keyboard)
  #:use-module (gnu system mapped-devices)
  #:use-module (gnu system shadow)
  #:use-module ((my systems common) #:prefix my:)
  #:export (%cocytus))

(define %cocytus
  (operating-system
    (inherit my:%operating-system)

    (host-name "cocytus")

    (locale "en_US.utf8")
    (timezone "Europe/Zurich")
    (keyboard-layout (keyboard-layout "us" "intl"))

    (kernel-arguments (cons* "fbcon=rotate:1" %default-kernel-arguments))

    (bootloader (bootloader-configuration
                 (inherit my:%bootloader-configuration)
                 (keyboard-layout keyboard-layout)))

    (mapped-devices
     (list
      (mapped-device (source (uuid "7988b88f-c1f8-4e76-8f82-6cfca62bd8d6")) ; Only UUIDs are supported yet.
                     (targets '("encrypted"))
                     (type luks-device-mapping))
      (mapped-device (source "system")
                     (targets '("system-swap" "system-root"))
                     (type lvm-device-mapping))))
    (swap-devices
     (list (swap-space (target (file-system-label "swap"))
                       (dependencies mapped-devices))))
    (file-systems
     (append
      (list
       (file-system (mount-point "/boot/efi")
                    (device (file-system-label "EFI"))
                    (type "vfat"))
       (file-system (mount-point "/")
                    (device (file-system-label "root"))
                    (type "ext4")
                    (dependencies mapped-devices)))
      %base-file-systems))

    (services
     (append
      (list
       (service dhcp-client-service-type) ; A simple DHCP client is enough, it's a wired machine.
       (bluetooth-service #:auto-enable? #t)) ; Trying out the desktop...
      my:%desktop-services
      (my:console-services #:font (file-append font-terminus "/share/consolefonts/ter-132n"))
      my:%minimal-services))

    (setuid-programs
     (append my:%desktop-setuid-programs %setuid-programs))

    (users
     (cons*
      (user-account
       (name "ether")
       ;; No supplementary-groups needed with (e)logind (e.g.: audio or video).
       ;; See loginctl seat-status.
       (group "users"))
      %base-user-accounts))))

%cocytus
