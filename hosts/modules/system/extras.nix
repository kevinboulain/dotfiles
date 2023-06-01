{ pkgs, ... }: {
  environment.systemPackages = with pkgs; [
    dmidecode
    pciutils
    usbutils
  ];
}
