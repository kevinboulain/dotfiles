(define-module (my packages bootloaders)
  #:use-module ((gnu packages bootloaders) #:prefix bootloaders:)
  #:use-module ((guix packages)))

;; https://jpoiret.xyz/luks2.tar.xz
;; https://lists.gnu.org/archive/html/grub-devel/2021-12/msg00085.html
(define-public grub-efi
  (let* ((patches-directory (string-append (canonicalize-path (dirname (current-filename))) "/patches/grub-efi/"))
         (patches (map (lambda (filename) (string-append patches-directory filename))
                       '("0001-devmapper-getroot-Have-devmapper-recognize-LUKS2.patch"
                         "0002-devmapper-getroot-Set-up-cheated-LUKS2-cryptodisk-mo.patch"))))
    (package/inherit bootloaders:grub-efi
     (source
      (origin
       (inherit (package-source bootloaders:grub-efi))
       (patches (append patches (origin-patches (package-source bootloaders:grub-efi)))))))))
