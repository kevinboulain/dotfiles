(define-module (my services desktop)
  #:use-module (guix gexp)
  #:use-module (guix records)
  #:use-module (gnu packages freedesktop)
  #:use-module (gnu services)
  #:use-module (gnu services base)
  #:use-module (gnu services shepherd)
  #:use-module (gnu system file-systems)
  #:use-module (gnu system pam)
  #:use-module ((my packages desktop) #:prefix my:)
  #:use-module (ice-9 match)
  #:export (pam-dumb-runtime-dir-service-type
            seatd-configuration
            seatd-service-type))

(define (pam-dumb-runtime-dir-file-systems _)
  ;; Stolen from @code{%elogind-file-systems} to ensure /run/user is cleaned on
  ;; reboot (and only on reboot).
  (list (file-system
         (device "none")
         (mount-point "/run/user")
         (type "tmpfs")
         (check? #f)
         (flags '(no-suid no-dev no-exec))
         (options "mode=0755")
         (create-mount-point? #t))))

(define (pam-dumb-runtime-dir-pam-service _)
  (define pam-dumb-runtime-dir
    (pam-entry
     (control "optional")
     (module (file-append my:dumb-runtime-dir "/lib/security/pam_dumb_runtime_dir.so"))))
  (list
   (lambda (pam)
     ;; Install for all services (or use @code{pam-service-name} to select
     ;; them).
     (pam-service
      (inherit pam)
      (session (cons* pam-dumb-runtime-dir (pam-service-session pam)))))))

(define pam-dumb-runtime-dir-service-type
  (service-type
   (name 'pam-dumb-runtime-dir)
   (extensions
    (list
     (service-extension pam-root-service-type pam-dumb-runtime-dir-pam-service)
     (service-extension file-system-service-type pam-dumb-runtime-dir-file-systems)))
   (default-value #nil)))

;; TODO: https://issues.guix.gnu.org/49969

(define-record-type* <seatd-configuration>
  seatd-configuration make-seatd-configuration seatd-configuration?
  (seatd seatd-package (default seatd))
  (user seatd-user (default "root"))
  (group seatd-group (default "users"))
  ;; This location doesn't grant users removal access.
  (socket seatd-socket (default "/run/seatd.sock")))

(define (seatd-shepherd-service configuration)
  (list
   (shepherd-service
    (documentation "Minimal seat management daemon")
    (provision '(seatd))
    (start
     #~(make-forkexec-constructor
        (list #$(file-append (seatd-package configuration) "/bin/seatd")
              "-u" #$(seatd-user configuration)
              "-g" #$(seatd-group configuration)
              "-s" #$(seatd-socket configuration))))
    (stop #~(make-kill-destructor)))))

(define seatd-environment
  (match-lambda
    (($ <seatd-configuration> _ _ _ socket)
     `(("SEATD_SOCK" . ,socket))))) ; For libseat.

(define seatd-service-type
  (service-type
   (name 'seatd)
   (description "Seat management takes care of mediating access to shared
devices (graphics, input), without requiring the applications needing access
to be root.")
   (extensions
    (list
     (service-extension session-environment-service-type seatd-environment)
     (service-extension shepherd-root-service-type seatd-shepherd-service)))
   (default-value (seatd-configuration))))
