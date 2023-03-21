{ config, myLib, mySystemDirectory, pkgs, ... }:
let
  inherit (myLib) mount;
in
{
  boot.initrd = {
    availableKernelModules = [
      # To mount the root filesystem.
      "nvme"
      # Everything else pretty much just works (including the keyboard).
    ];

    # Sets up /dev/mapper/root.
    luks.devices.root.keyFile = "/root.key";
    # A LUKS key to avoid entering the passphrase twice. This is safe as long as
    # the boot partition is encrypted (the resulting initrd has restricted
    # permissions so users other than root can't peek).
    # I wonder how much of a bad idea it would be to use the SSH host key:
    # https://man.archlinux.org/man/cryptsetup.8.en#Passphrase_processing_for_LUKS
    # > The complete keyfile is read up to the compiled-in maximum size.
    # > Newline characters do not terminate the input.
    secrets."/root.key" = "${mySystemDirectory}/etc/luks/root.key";
  };

  fileSystems = {
    "/boot" = mount.btrfs {
      device = "/dev/mapper/root";
      subvolume = "system/boot";
    };
    "/boot/efi".depends = [ "/boot" ];
    "/boot/rescue" = {
      fsType = "ext4";
      depends = [ "/boot" ];
    };
    "/swap" = mount.btrfs {
      device = "/dev/mapper/root";
      subvolume = "system/swap";
    };
  };

  boot.resumeDevice = "/dev/mapper/root";
  swapDevices = [{
    device = "/swap/swap";
    # Setting the size would generate a service that would try to create the
    # swap file when it doesn't match. That is unlikely to do what we expect on
    # Btrfs.
  }];

  environment.systemPackages = with pkgs; [
    # https://01.org/linuxgraphics/documentation/development/how-debug-suspend-resume-issues
    intel-gpu-tools
  ];

  # Wi-Fi driver was merged somewhen between 5.15 and 5.17.
  # Hibernation has been broken between 5.17.15 and 6.0.3:
  # https://gitlab.freedesktop.org/drm/intel/-/issues/6506
  # Audio has been broken between 6.0.3 and 6.0.4:
  # https://bugzilla.kernel.org/show_bug.cgi?id=216613
  boot.kernelPackages = pkgs.linuxPackages;

  # Tells ccache to set up the environment for a package:
  # https://github.com/NixOS/nixpkgs/blob/master/nixos/modules/programs/ccache.nix#L58
  # programs.ccache.packageNames = [ "kernel" ];
  # A custom kernel package to ease bisection.
  # https://euank.com/2022/05/11/nixos-bisect.html
  # https://nixos.wiki/wiki/Linux_kernel
  # boot.kernelPackages =
  #   let
  #     kernelPackage = { fetchurl, buildLinux, lib, ... }@args:
  #       buildLinux (args // rec {
  #         # Additionally, the environment needs to be overridden with ccache's.
  #         # https://github.com/NixOS/nixpkgs/issues/153343
  #         stdenv = pkgs.ccacheStdenv;
  #         # To fetch the source from somewhere:
  #         src = fetchurl {
  #           # If the method and parameters are the same (the hash changes
  #           # depending on the method) we can hit cache.nixos.org.
  #           url = "mirror://kernel/linux/kernel/v6.x/linux-${version}.tar.xz";
  #           sha256 = "17awx4c5fz7f656ig5bydccci052jsai0lczrn2bdk5cihw2cg51";
  #         };
  #         # Or instead, to copy it locally (will always cause ccache misses?):
  #         # https://nixos.org/manual/nix/stable/expressions/builtins.html#builtins-filterSource
  #         # src = builtins.filterSource
  #         #   (path: type: type != "directory" || baseNameOf path != ".git")
  #         #   /home/ether/sources/linux;
  #         version = "6.0.2";
  #         modDirVersion = builtins.replaceStrings ["-"] [".0-"] version;
  #         kernelPatches = with (pkgs.callPackage <nixos/pkgs/os-specific/linux/kernel/patches.nix> {}); [
  #           # https://github.com/NixOS/nixpkgs/blob/master/pkgs/top-level/linux-kernels.nix
  #           bridge_stp_helper
  #           request_key_helper
  #           { name = "https://gitlab.freedesktop.org/drm/intel/-/issues/6506";
  #             patch = ./drm_resume.patch; }
  #         ];
  #         # Sometimes it's too much work to fiddle with structuredExtraConfig
  #         # when incompatible options are set.
  #         # ignoreConfigErrors = true;
  #         # structuredExtraConfig = with lib.kernel; {};
  #       } // (args.argsOverride or {}));
  #     kernel = pkgs.callPackage kernelPackage {};
  #   in pkgs.recurseIntoAttrs (pkgs.linuxPackagesFor kernel);

  # Power management.
  # TODO: suspend-then-hibernate is buggy but worked fine on Arch IIRC, here's
  # an excerpt:
  #  sdhci-pci 0000:6e:00.0: Unable to change power state from D3hot to D0, device inaccessible
  #  xhci_hcd 0000:3a:00.0: xHC error in resume, USBSTS 0x401, Reinit
  #  i2c_hid_acpi i2c-ELAN0406:00: i2c_hid_get_input: incomplete report (14/3583)
  #  i2c_hid_acpi i2c-ELAN0406:00: i2c_hid_get_input: IRQ triggered but there's no data
  #  mmc0: Reset 0x1 never completed.
  #  mmc0: Controller never released inhibit bit(s).
  #  mmc0: sdhci: ============ SDHCI REGISTER DUMP ===========
  #  mmc0: 3.3V regulator output did not become stable
  #  nvidia-modeset: ERROR: GPU:0: Failed detecting connected display devices
  services.logind.lidSwitch = "hibernate";
  systemd.sleep.extraConfig = ''
    # https://wiki.archlinux.org/title/Power_management#Hybrid-sleep_on_suspend_or_hibernation_request
    SuspendMode=suspend
    SuspendState=disk
    HibernateMode=shutdown
    HibernateState=disk
    # Default is 2h.
    HibernateDelaySec=5m
  '';
  services.upower = {
    enable = true;
    percentageLow = 20;
    percentageCritical = 10;
    percentageAction = 5;
    criticalPowerAction = "Hibernate";
  };
  systemd.services.upower.wantedBy = [ "multi-user.target" ];  # By default it's graphical.target.
  # The rest (power saving for the audio interface, automatic power management
  # of PCI devices, etc) is handled by TLP
  # (https://linrunner.de/tlp/faq/powertop.html):
  services.tlp = {
    enable = true;
    settings = {
      # Not enabled by default.
      # https://linrunner.de/tlp/settings/processor.html
      CPU_SCALING_GOVERNOR_ON_AC = "powersave";
      CPU_SCALING_GOVERNOR_ON_BAT = "powersave";
    };
  };
  powerManagement.resumeCommands = ''
    # TODO: for some reason the power button is disabled after hibernation.
    echo enable > /sys/firmware/acpi/interrupts/ff_pwr_btn
  '';

  hardware = {
    opengl.enable = true;
    video.hidpi.enable = true;  # Also impacts the console.
    # This system has an Nvidia graphic card.
    # Driver 520.56.06 fixes the diagonal tearing in XWayland under PRIME:
    # https://gitlab.freedesktop.org/xorg/xserver/-/issues/1317
    nvidia = {
      # There are way more options, like explicitly setting PCI buses:
      # https://github.com/NixOS/nixpkgs/blob/master/nixos/modules/hardware/video/nvidia.nix
      # But most of them are configuring Xorg or the power state but the
      # hardware seems perfectly capable to hibernate, enter the D3cold state
      # and offload stuff like games to the proper GPU.
      # To force an application to use a GPU (e.g.: glxgears), one might need to
      # set a few variables:
      # https://wiki.archlinux.org/title/PRIME#Configure_applications_to_render_using_GPU
      modesetting.enable = true;
      # The default.
      package = config.boot.kernelPackages.nvidiaPackages.stable;
      # Largely copied from
      # https://github.com/NixOS/nixpkgs/blob/master/pkgs/os-specific/linux/nvidia-x11/default.nix
      # I didn't manage to use an overlay:
      # https://github.com/NixOS/nixpkgs/issues/90459#issuecomment-647041204
      # package = let
      #   generic = args: let imported = import <nixos/pkgs/os-specific/linux/nvidia-x11/generic.nix> args;
      #                   in pkgs.callPackage imported {
      #                     # Build against the current kernel.
      #                     kernel = config.boot.kernelPackages.kernel;
      #                     lib32 = (pkgsi686Linux.callPackage imported {
      #                       libsOnly = true;
      #                       kernel = null;
      #                     }).out;
      #                   };
      # in generic {
      #   version = "515.65.01";
      #   sha256_64bit = "sha256-BJLdxbXmWqAMvHYujWaAIFyNCOEDtxMQh6FRJq7klek=";
      #   openSha256 = "sha256-GCCDnaDsbXTmbCYZBCM3fpHmOSWti/DkBJwYrRGAMPI=";
      #   settingsSha256 = "sha256-kBELMJCIWD9peZba14wfCoxsi3UXO3ehFYcVh4nvzVg=";
      #   persistencedSha256 = "sha256-P8oT7g944HvNk2Ot/0T0sJM7dZs+e0d+KwbwRrmsuDY=";
      #   patches = [ ./drm_nvidia.patch ];
      # };
    };
  };
  # While it's named after X, it doesn't install it. This is necessary
  # to enable the Nvidia graphic card.
  services.xserver.videoDrivers = [ "nvidia" ];

  # Not strictly necessary but more appealing.
  boot.loader.grub.gfxmodeEfi = "1920x1080";
}
