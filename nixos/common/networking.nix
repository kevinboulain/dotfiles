{ pkgs, ... }:
{
  environment.systemPackages = with pkgs; [
    iw
    mtr
    tcpdump
  ];

  networking.firewall = {
    enable = true;
    logRefusedPackets = true;  # Wasted my time debugging dropped packets...
  };

  # DNS resolution is handed off to systemd.
  services.resolved.enable = true;
  # And enable mDNS (discovery of printers, chromecasts, ...).
  services.avahi = {
    enable = true;
    nssmdns = true;
    publish = {
      enable = true;
      addresses = true;
    };
  };

  services.openssh = {
    enable = true;
    extraConfig = ''
      # Only allow users with privileges to log in remotely.
      AllowGroups root wheel
    '';
  };
}
