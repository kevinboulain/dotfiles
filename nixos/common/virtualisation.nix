# qemu-kvm -display curses -netdev bridge,br=nat0,id=net0,helper="$(which qemu-bridge-helper)" -device virtio-net-pci,netdev=net0 -m 1024M -drive file=debian-live.iso,media=cdrom
{ pkgs, ... }:
{
  environment.systemPackages = with pkgs; [
    qemu
  ];

  environment.etc."qemu/bridge.conf" = {
    mode = "0644";
    text = ''
      allow nat0
    '';
  };
  security.wrappers.qemu-bridge-helper = {
    capabilities = "cap_net_admin+p";
    owner = "root";
    group = "root";
    source = "${pkgs.qemu}/libexec/qemu-bridge-helper";
  };
}
