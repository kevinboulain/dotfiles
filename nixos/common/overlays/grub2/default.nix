# https://discourse.nixos.org/t/is-there-grub-patched-for-booting-from-partition-encrypted-with-luks2/18398
self: super: {
  grub2 = super.grub2.overrideAttrs (oldAttrs: {
    patches = oldAttrs.patches ++ [./grub-install_luks2.patch];
  });
}
