{ config, pkgs, pkgsi686Linux, ... }:
{
  fileSystems."/boot/efi".device = "/dev/disk/by-uuid/8EEA-1769";

  environment.systemPackages = with pkgs; [
    # https://01.org/linuxgraphics/documentation/development/how-debug-suspend-resume-issues
    intel-gpu-tools
  ];

  # This system has a recent Wi-Fi card. 5.15 is known to be missing the
  # driver while 5.17 is known to have it (including the firmware).
  # 5.17.15 is the last version that allows waking up from hibernation:
  # https://gitlab.freedesktop.org/drm/intel/-/issues/6506
  # boot.kernelPackages = pkgs.linuxPackages_latest;

  # Tells ccache to set up the environment for a package:
  # https://github.com/NixOS/nixpkgs/blob/master/nixos/modules/programs/ccache.nix#L58
  programs.ccache.packageNames = [ "kernel" ];
  # A custom kernel package to ease bisection.
  # https://euank.com/2022/05/11/nixos-bisect.html
  # https://nixos.wiki/wiki/Linux_kernel
  boot.kernelPackages = let
    kernelPackage = { fetchurl, buildLinux, lib, ... }@args:
      buildLinux (args // rec {
        # Additionally, the environment needs to be overridden with ccache's.
        # https://github.com/NixOS/nixpkgs/issues/153343
        stdenv = pkgs.ccacheStdenv;
        # To fetch the source from somewhere:
        src = fetchurl {  # If the method and parameters are the same (the hash changes depending on the method) we can hit cache.nixos.org.
          url = "https://cdn.kernel.org/pub/linux/kernel/v5.x/linux-${version}.tar.xz";
          sha256 = "sha256-ShySKkkO6r9bRNT9423pultxcRtzUsYlhxbaQRYNtig=";
        };
        # Or instead, to copy it locally (will always cause ccache misses?):
        # https://nixos.org/manual/nix/stable/expressions/builtins.html#builtins-filterSource
        # src = builtins.filterSource
        #   (path: type: type != "directory" || baseNameOf path != ".git")
        #   /home/ether/source/linux;
        version = "5.17.15";
        kernelPatches = with (pkgs.callPackage <nixos/pkgs/os-specific/linux/kernel/patches.nix> {}); [
          # https://github.com/NixOS/nixpkgs/blob/master/pkgs/top-level/linux-kernels.nix
          bridge_stp_helper
          request_key_helper
        ];
        # Sometimes it's too much work to fiddle with structuredExtraConfig
        # when incompatible options are set.
        # ignoreConfigErrors = true;
        # structuredExtraConfig = with lib.kernel; {};
      } // (args.argsOverride or {}));
    kernel = pkgs.callPackage kernelPackage {};
  in pkgs.recurseIntoAttrs (pkgs.linuxPackagesFor kernel);

  boot = {
    kernelParams = [];
    initrd = {
      luks.devices.root.device = "/dev/disk/by-uuid/896ef078-adb2-4405-afb8-ec62ea116399";
      availableKernelModules = [
        # To mount the root filesystem.
        "nvme"
        # Everything else pretty much just works (including the keyboard).
      ];
    };
  };

  # Power management.
  boot.resumeDevice = "/dev/system/swap";
  services.logind.lidSwitch = "hibernate"; # TODO: suspend-then-hibernate (buggy right now, worked fine on arch)
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

  hardware = {
    opengl.enable = true;
    video.hidpi.enable = true;  # Also impacts the console.
    nvidia = {  # This system has an Nvidia graphic card.
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
      # package = config.boot.kernelPackages.nvidiaPackages.stable;
      # Largely copied from
      # https://github.com/NixOS/nixpkgs/blob/master/pkgs/os-specific/linux/nvidia-x11/default.nix
      package = let
        generic = args: let imported = import <nixos/pkgs/os-specific/linux/nvidia-x11/generic.nix> args;
                        in pkgs.callPackage imported {
                          # Build against the current kernel.
                          kernel = config.boot.kernelPackages.kernel;
                          lib32 = (pkgsi686Linux.callPackage imported {
                            libsOnly = true;
                            kernel = null;
                          }).out;
                        };
      in generic {
        version = "515.48.07";
        sha256_64bit = "sha256-4odkzFsTwy52NwUT2ur8BcKJt37gURVSRQ8aAOMa4eM=";
        settingsSha256 = "sha256-XwdMsAAu5132x2ZHqjtFvcBJk6Dao7I86UksxrOkknU=";
        persistencedSha256 = "sha256-BTfYNDJKe4tOvV71/1JJSPltJua0Mx/RvDcWT5ccRRY=";
      };
    };
  };
  # While it's named after X, it doesn't install it. This is necessary
  # to enable the Nvidia graphic card.
  services.xserver.videoDrivers = [ "nvidia" ];

  # Not strictly necessary but more appealing.
  boot.loader.grub.gfxmodeEfi = "1920x1080";

  # Steam.
  programs.steam.enable = true;  # Installing via nix-env doesn't work: "libnvidia-glvkspirv.so is required at runtime".
  hardware.opengl.driSupport32Bit = true;
  programs.gamemode.enable = true;  # For performance-sensitive applications.
}
