{ ... }: {
  # Because usbcore.authorized_default=0 is set, ensure the keyboard
  # is available early:
  #  - in case the disk needs to be unlocked,
  #  - weirdly, if it's only authorized later on, the backlight
  #    doesn't stay always on.
  # While it can't be as precise as USBGuard's hash, because the
  # default policy is to block, it also needs to be authorized by
  # it.
  boot.initrd.services.udev.rules = ''
    SUBSYSTEMS=="usb", ACTION=="add", KERNEL=="1-8", ATTR{removable}=="fixed", ATTR{idVendor}=="1532", ATTR{idProduct}=="028a", ATTR{authorized}="1"
  '';

  services.usbguard.rules = ''
    allow label "hub" via-port one-of { "usb1" "usb2" } hash one-of { "jEP/6WzviqdJ5VSeTUY8PatCNBKeaREvo2OqdplND/o=" "prM+Jby/bFHCn2lNjQdAMbgc6tse3xVx+hZwjOPHSdQ=" } parent-hash "rV9bfLq7c2eA4tYjVjwO4bxhm+y6GgZpl9J60L0fBkY="
    allow label "hub" via-port one-of { "usb3" "usb4" } hash one-of { "2Q98D37MPBQTdJ5DxCz5xj1ySFrvVTViBxFXII6DDxM=" "jglhZsL6H8wRgDqv95gpUlY50uWl7BxQpokfdB3TRuk=" } parent-hash "vkZowmFpJPUT9Nbt2VOGXwBsp+f37ICsMIGYs46YwWA="

    # Permanent.
    block label "camera" via-port "1-2" with-connect-type "hardwired" hash "nC6QMwlPaWjRwMETTjQFP7Ea0yYW/rTavevUoqW43SI=" parent-hash "jEP/6WzviqdJ5VSeTUY8PatCNBKeaREvo2OqdplND/o="
    allow label "keyboard" via-port "1-8" with-connect-type "hardwired" hash "uBkasyMicWnOtJn08hmMh86D1Hf6CAIH5I78a9G35KM=" parent-hash "jEP/6WzviqdJ5VSeTUY8PatCNBKeaREvo2OqdplND/o="
    allow label "bluetooth" via-port "1-10"  with-connect-type "hardwired" hash "ciwwGozaSw4maEXfs4NdvETeMt6bnFEK6f4vmCqfud0=" parent-hash "jEP/6WzviqdJ5VSeTUY8PatCNBKeaREvo2OqdplND/o="

    # Semi-permanent.
    allow label "yubikey" via-port "1-4" with-connect-type "hotplug" hash "vTQDxYra1S117L9vQ5ccQRR6oQs4kBp2oXcZtvDcCSM=" parent-hash "jEP/6WzviqdJ5VSeTUY8PatCNBKeaREvo2OqdplND/o="
  '';
}
