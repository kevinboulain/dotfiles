;; guix system --dry-run --load-path="$(pwd)" image --image-type=iso9660 my/systems/installation.scm

(define-module (my systems installation)
  #:use-module (guix gexp)
  #:use-module (guix profiles)
  #:use-module (guix describe)
  #:use-module (gnu bootloader)
  #:use-module (gnu bootloader grub)
  #:use-module (gnu services)
  #:use-module (gnu services networking)
  #:use-module (gnu system)
  #:use-module (gnu system file-systems)
  #:use-module (gnu system image)
  #:use-module (gnu system keyboard)
  #:use-module (gnu system linux-initrd)
  #:use-module (gnu system pam)
  #:use-module ((my channels) #:prefix my:)
  #:use-module ((my systems common) #:prefix my:)
  #:use-module ((my systems acheron) #:prefix my:)
  #:use-module ((my systems cocytus) #:prefix my:)
  #:use-module ((my systems phlegethon) #:prefix my:))

(define %installation-files
  (list
   ;; Copy ../../my to /etc/installation/my to ease a new system installation.
   `("installation/my"
     ,(local-file
       (string-append (canonicalize-path (dirname (current-filename))) "/..")
       "my" #:recursive? #t))
   ;; Guix runs in an isolated environment, called profile, in which builds
   ;; should be performed and the profile can easily be rebuilt with a simple
   ;; @code{guix pull} (assuming there is provenance information).
   ;; Based on that, @code{guix system image} doesn't try to embed channel code
   ;; but I'd like it be readily available so I can @code{guix system init}
   ;; systems without having to @code{guix pull} first.
   ;;
   ;; @code{guix pull} roughly do something like this:
   ;;  (let* ((channels ...)
   ;;         (instances (run-with-store (open-connection) ((store-lift latest-channel-instances) channels)))
   ;;         (manifest (run-with-store (open-connection) (channel-instances->manifest instances))))
   ;;    (profile-derivation manifest)) ; From @code{(guix profiles)}.
   ;;
   ;; The problem is that it's costly (@code{instances} clones the channels
   ;; while @code{manifest} computes the Guix derivation) and doesn't guarantee
   ;; it's the same profile as the @code{guix system image} building this image.
   ;; Fortunately, all the necessary pieces are available:
   ;;  - @code{profile-compiler} returns a gexp from the above derivation (it's
   ;;    a @code{define-gexp-compiler}),
   ;;  - @code{current-profile} returns the current profile in use, from which
   ;;    the manifest can be inferred with @code{profile-manifest}.
   ;;
   ;; Activate the profile with:
   ;;   GUIX_PROFILE=/etc/installation/profile
   ;;   . "$GUIX_PROFILE"/etc/profile
   `("installation/profile"
     ,(profile (content (profile-manifest (current-profile)))))))

;; Keep references to these objects to ensure everything we might need is
;; readily available. It's also supposed to prevent downloading and rebuilding
;; (see comments in @code{(gnu system install)}) but I haven't managed to
;; reproduce that.
(define %installation-references
  (list
   ;; parted --script /dev/sda mklabel gpt
   ;; parted --script --align optimal /dev/sda mkpart EFI 1MiB 200MiB
   ;; parted /dev/sda set 1 esp on
   ;; mkfs.fat -n EFI -F32 /dev/sda1
   ;; parted --script --align optimal /dev/sda mkpart encrypted 200MiB 100%
   ;; cryptsetup luksFormat --type luks2 --pbkdf pbkdf2 /dev/sda2 # Argon doesn't appear to be supported yet.
   ;; cryptsetup luksUUID /dev/sda2 --uuid 00000000-0000-0000-0000-000000000000
   ;; cryptsetup open /dev/sda2 encrypted
   ;; pvcreate /dev/mapper/encrypted
   ;; vgcreate system /dev/mapper/encrypted
   ;; lvcreate --size 8GiB system --name swap
   ;; mkswap --label swap /dev/system/swap
   ;; swapon -L swap
   ;; lvcreate --extents 100%free system --name root
   ;; mkfs.ext4 -L root /dev/system/root
   ;; mount --label root --target /mnt
   ;; mkdir -p /mnt/boot/efi
   ;; mount --label EFI --target /mnt/boot/efi
   ;;
   ;; GUIX_PROFILE=/etc/installation/profile
   ;; . "$GUIX_PROFILE"/etc/profile
   ;; guix system --load-path=/etc/installation init /etc/installation/my/systems/cocytus.scm /mnt
   my:%acheron my:%cocytus my:%phlegethon))

(operating-system
 (inherit my:%operating-system)
 (locale "en_US.utf8")
 (timezone "Europe/Zurich")
 (keyboard-layout (keyboard-layout "us" "intl"))
 (host-name "installation")
 (services
  (append
   (list
    (simple-service 'installation-files etc-service-type %installation-files)
    (service gc-root-service-type %installation-references)
    ;; Avahi requires a networking service, a simple DHCP could be enough and
    ;; if it's not, the systems we're trying to install migh have the necessary
    ;; utilities (hence why we embed them).
    (service dhcp-client-service-type))
   (my:console-services)
   my:%minimal-services))
 (pam-services
  (base-pam-services #:allow-empty-passwords? #t))
 (bootloader
  (bootloader-configuration
   (bootloader grub-bootloader)
   (targets '("/dev/sda"))
   (keyboard-layout keyboard-layout)))
 (initrd base-initrd) ; TODO: nonguix's combined initrd can't be booted from the ISO.
 (file-systems
  (cons* (file-system
          (mount-point "/")
          (device (file-system-label root-label))
          (type "ext4"))
         %base-file-systems)))
