{ pkgs, ... }: {
  services.usbguard = {
    enable = true;

    # Always apply the policy (even for already connected devices) and block by
    # default.
    implictPolicyTarget = "block";  # Typo...
    presentDevicePolicy = "apply-policy";
    presentControllerPolicy = "apply-policy";
    insertedDevicePolicy = "apply-policy";

    # Only allow users with privileges to authorize USB devices.
    IPCAllowedUsers = [ "root" ];
    IPCAllowedGroups = [ "wheel" ];
  };
  boot.kernelParams = [
    # To avoid any race, disable all USB devices at boot. Use
    # boot.initrd.services.udev.rules when some of them are necessary in the
    # initrd (for example, the keyboard to unlock the disk).
    "usbcore.authorized_default=0"
    # And if USBGuard is misbehaving, it's possible to recover from GRUB via
    # systemd.mask=usbguard.service
  ];

  systemd.packages = with pkgs; [
    usbguard-notifier
  ];
  # https://nixos.org/manual/nixos/stable/index.html#sect-nixos-systemd-nixos
  # User services aren't automatically started.
  systemd.user.targets.default.wants = [ "usbguard-notifier.service" ];
}
