```shell
parted --script /dev/sda mklabel gpt
parted --script --align optimal /dev/sda mkpart EFI 1MiB 200MiB
parted /dev/sda set 1 esp on
mkfs.fat -n EFI -F32 /dev/sda1
parted --script --align optimal /dev/sda mkpart encrypted 200MiB 100%
cryptsetup luksFormat --type luks2 --pbkdf pbkdf2 /dev/sda2 # Argon doesn't appear to be supported yet.
# cryptsetup luksUUID /dev/sda2 --uuid 00000000-0000-0000-0000-000000000000
cryptsetup open /dev/sda2 encrypted
pvcreate /dev/mapper/encrypted
vgcreate system /dev/mapper/encrypted
lvcreate --size 8GiB system --name swap
mkswap --label swap /dev/system/swap
swapon -L swap
lvcreate --extents 100%free system --name root
mkfs.ext4 -L root /dev/system/root
mount --label root --target /mnt
mkdir -p /mnt/boot/efi
mount --label EFI --target /mnt/boot/efi
```

Note: keep room for `/dev/system/prometheus` if you need it.

```shell
nixos-generate-config --root /mnt
mkdir --mode 0700 /mnt/etc/secrets
dd if=/dev/urandom of=/mnt/etc/secrets/root.key bs=1024 count=4
chmod 0400 /mnt/etc/secrets/root.key
cryptsetup luksAddKey /dev/sda2 /mnt/etc/secrets/root.key
# clone and symlink the configuration of your choice in /mnt/etc/nixos/configuration.nix
nixos-install
```
