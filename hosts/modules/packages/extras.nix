{ config, pkgs, ... }: {
  # https://discourse.nixos.org/t/missing-man-pages/4680/6
  # Some, but not all, of these are true by default.
  documentation = {
    enable = true;
    dev.enable = true;
    man.enable = true;
  };

  environment.systemPackages = with pkgs; [
    man-pages
    man-pages-posix

    config.boot.kernelPackages.perf
    file
    lsof
    strace
    unzip
  ];
}
