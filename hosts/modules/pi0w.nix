# To start from scratch:
#  - download and flash an image from
#    https://www.raspberrypi.com/software/operating-systems/
#  - edit config.txt to enable serial and USB gadgets:
#     +enable_uart=1
#     +dtoverlay=dwc2
#  - edit cmdline.txt to disable the serial console over GPIO and enable the USB
#    serial gadget:
#     -console=serial0,115200
#     +modules-load=dwc2,g_serial
#  - enable getty:
#     sudo ln -s /lib/systemd/system/getty@.service .../etc/systemd/system/getty.target.wants/getty@ttyGS0.service
#  - unlock the pi account:
#     mkpasswd --method=SHA-512
#     $EDITOR .../etc/shadow
#  - once connected, run raspi-config (to set up Wi-Fi, SSH, ...).
# From there, it's possible to connect another Pi via the serial over GPIO to
# debug issues: edit config.txt similarly and edit cmdline.txt but keep the
# console.
{ config, nixpkgs, pkgs, ... }: {
  # nixpkgs supports at least two different ways to set up a Raspberry Pi image:
  #  nixos/modules/installer/sd-card/sd-image-raspberrypi.nix
  #  nixos/modules/system/boot/loader/raspberrypi/raspberrypi.nix
  # The first is U-Boot only, which supposedly can pass the DTB but, from what I
  # can read from a few issues, isn't really supported in NixOS (and it's not
  # like I want to compile stuff on the Pi itself given the absence of
  # substitutes).
  # The second appears to be a bit more configurable but, for some reason,
  # uboot-builder.sh and raspberrypi-builder.sh are expected to be run from the
  # hostPlatform.system and require:
  #  boot.binfmt.emulatedSystems = [ "armv6l-linux" ];
  # Both hardcode a part of config.txt and don't copy overlays. See also:
  #  https://discourse.nixos.org/t/rasperry-pi-3-with-official-firmware-and-bootloader/24533
  #  https://github.com/NixOS/nixpkgs/issues/67792
  #  https://github.com/NixOS/nixpkgs/issues/125354
  #
  # In light of all this, set up the bootloader ourselves. I expect the Pi will
  # always be reflashed and not updated in place.
  imports = [
    # nix build .#nixosConfigurations.${HOSTNAME?}.config.system.build.sdImage
    # zstd -dc result/sd-image/nixos-sd-image-*-armv6l-linux.img.zst | sudo dd of=/dev/mmcblk0 bs=1M
    "${nixpkgs}/nixos/modules/installer/sd-card/sd-image.nix"
  ];

  sdImage = with config.system.build; {
    firmwareSize = 256;  # MiB
    populateFirmwareCommands =
      let
        initrd = "initrd";
        configTxt = pkgs.writeText "config.txt" ''
          # By disabling the Bluetooth, UART0 (PL011/ttyAMA0) takes over GPIO 14
          # & 15 instead of UART1 (mini UART/ttyS0). The mini UART has
          # drawbacks, like requiring a fixed CPU frequency. UART0 is enabled by
          # default but not UART1 so this is unnecessary (and actually
          # harmful?).
          # enable_uart=1
          dtoverlay=disable-bt

          # For gadgets, like g_serial or g_ether.
          dtoverlay=dwc2

          initramfs ${initrd} followkernel
        '';
      in
        ''
          out="$NIX_BUILD_TOP"/firmware

          # The bootloader and the firmwares.
          # https://www.raspberrypi.com/documentation/computers/configuration.html#boot-folder-contents
          cp ${pkgs.raspberrypifw}/share/raspberrypi/boot/{bootcode.bin,start.elf,fixup.dat} "$out"
          # https://www.raspberrypi.com/documentation/computers/raspberry-pi.html#bootcode-bin-uart-enable
          sed -i s/BOOT_UART=0/BOOT_UART=1/ "$out"/bootcode.bin
          # https://www.raspberrypi.com/documentation/computers/configuration.html#using-device-trees-on-raspberry-pi
          cp -r ${pkgs.raspberrypifw}/share/raspberrypi/boot/bcm2708-rpi-zero-w.dtb "$out"
          # https://www.raspberrypi.com/documentation/computers/configuration.html#the-overlays-folder
          # https://github.com/raspberrypi/firmware/blob/master/boot/overlays/README
          mkdir "$out"/overlays
          cp -r ${pkgs.raspberrypifw}/share/raspberrypi/boot/overlays/{dwc2.dtbo,disable-bt.dtbo} "$out"/overlays
          # https://www.raspberrypi.com/documentation/computers/config_txt.html
          cp ${configTxt} "$out"/config.txt
          # https://www.raspberrypi.com/documentation/computers/configuration.html#the-kernel-command-line
          printf "%s init=%s\n" "$(cat ${toplevel}/kernel-params)" "${toplevel}/init" > "$out"/cmdline.txt

          # The kernel itself.
          cp -L ${toplevel}/kernel "$out"/kernel.img
          cp -L ${toplevel}/initrd "$out"/${initrd}
        '';
    populateRootCommands = ''
      # By default, something similar is done from boot.postBootCommands (by
      # sd-card/sd-image.nix) but populateRootCommands is still expected to be
      # set.
      # mkdir -p ./files/nix/var/nix/profiles/
      # ln -s ${toplevel} ./files/nix/var/nix/profiles/system
    '';
  };

  boot = {
    loader.grub.enable = false;
    # Note this isn't mainline (from what I gathered from a few issues, it
    # wouldn't support vendor DTBs and overlays).
    kernelPackages = pkgs.linuxPackages_rpi0;
    # Obviously,
    #  kernelParams = [ "console=ttyAMA0,115200" ]
    # will conflict with any serial device and so isn't done here.
  };

  hardware.firmware = with pkgs; [
    raspberrypiWirelessFirmware
  ];

  # Unlikely to do any build on the machine itself so spare the SD card.
  boot.tmp.useTmpfs = true;
}
