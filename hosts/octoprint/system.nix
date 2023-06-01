{ myPublicKey, ... }: {
  # Serial console over USB.
  # Unsure why but g_serial can't be loaded from the initrd directly:
  #  udc 20980000.usb: failed to start g_serial: -2
  # It doesn't matter in practice because getty is started in stage 2.
  boot.kernelModules = [ "dwc2" "g_serial" ];
  # For some reason, console=ttyGS0,115200 on the kernel's command line won't
  # work.
  systemd.targets.getty.wants = [ "getty@ttyGS0.service" ];

  users = {
    mutableUsers = false;
    users.root = {
      # I don't think I care: root isn't allowed to log in via SSH with a
      # password and since the SD card isn't encrypted, physical access implies
      # it's compromised. This might change if I ever generate the partition
      # table myself.
      password = "root";
      openssh.authorizedKeys.keys = [ myPublicKey ];
    };
  };
}
