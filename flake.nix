{
  inputs = {
    home-manager = {
      url = "github:nix-community/home-manager";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    sops-nix = {
      url = "github:mic92/sops-nix";
      inputs.nixpkgs.follows = "nixpkgs";
      # Their nixpkgs-stable doesn't appear to be used outside tests, one less
      # dependency.
      inputs.nixpkgs-stable.follows = "nixpkgs";
    };
  };

  outputs = { home-manager, nixpkgs, sops-nix, ... }: {
    nixosConfigurations =
      let
        # Sadly, it doesn't look like there's an easy way to get the public key
        # encrypted with sops-nix: AuthorizedKeysFile would have to be tweaked
        # and that wouldn't solve interactive decryption in the initrd (the key
        # defaults to config.users.users.root.openssh.authorizedKeys.keys).
        myPublicKey = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIBY+O+8nOT5MAq6hxnYw0BxxcKQTHplbwQggkshktgUi";
        myStateDirectory = "/srv";
        mySystemDirectory = "${myStateDirectory}/system";
        specialArgs = rec {
          inherit myPublicKey myStateDirectory mySystemDirectory nixpkgs;
          myLib = import ./hosts/lib { inherit (nixpkgs) lib; inherit mySystemDirectory; };
        };
      in
        {
          acheron = nixpkgs.lib.nixosSystem {
            inherit specialArgs;
            system = "x86_64-linux";
            modules = [
              ./hosts/acheron
              ./hosts/modules/android.nix
              ./hosts/modules/desktop.nix
              ./hosts/modules/home-manager.nix
              ./hosts/modules/impermanence
              ./hosts/modules/impermanence/backup.nix
              ./hosts/modules/locale.nix
              ./hosts/modules/monitoring.nix
              ./hosts/modules/networking
              ./hosts/modules/networking/iwd.nix
              ./hosts/modules/nix
              ./hosts/modules/nix/ccache.nix
              ./hosts/modules/packages.nix
              ./hosts/modules/rescue.nix
              ./hosts/modules/steam.nix
              ./hosts/modules/system
              ./hosts/modules/system/efi.nix
              ./hosts/modules/system/users
              ./hosts/modules/system/users/untrusted.nix
              ./hosts/modules/usbguard.nix
              ./hosts/modules/virtualization.nix
              ./hosts/modules/yubikey.nix
              home-manager.nixosModules.home-manager
              sops-nix.nixosModules.sops
            ] ++ [{
              networking.hostName = "acheron";
              time.timeZone = "Europe/Paris";
              system.stateVersion = "22.11";

              fileSystems."/boot/efi".device = "/dev/disk/by-uuid/87AC-02CE";
              fileSystems."/boot/rescue".device = "/dev/disk/by-uuid/96b89522-deab-42d3-ab43-0040bbb0e47b";
              boot.rescue.isos = [ "nixos-minimal-23.05.1597.c7a18f89ef1-x86_64-linux.iso" ];
              boot.kernelParams = [
                # Run 'btrfs inspect-internal map-swapfile path/to/swap' to find out.
                # A full rebalance might break it:
                # https://bugzilla.kernel.org/show_bug.cgi?id=217066
                "resume_offset=26486016"
              ];
              boot.initrd.luks.devices.root.device = "/dev/disk/by-uuid/a8cb3a4c-3b49-44db-a476-fc02551063b3";

              home-manager = {
                extraSpecialArgs.myLib = import ./homes/lib { inherit (nixpkgs) lib; };
                users =
                  let
                    home.stateVersion = "22.11";
                    wayland.windowManager.sway.hiDPIFix = true;
                  in
                    {
                      root = { ... }: {
                        inherit home;
                        imports = [ ./homes/minimal.nix ];
                      };
                      ether = { ... }: {
                        inherit home wayland;
                        imports = [
                          ./homes/desktop.nix
                          ./homes/modules/yubikey.nix
                        ];
                      };
                      untrusted = { ... }: {
                        inherit home wayland;
                        imports = [ ./homes/desktop.nix ];
                      };
                    };
              };
            }];
          };

          node-01 = nixpkgs.lib.nixosSystem {
            inherit specialArgs;
            system = "aarch64-linux";
            modules = [
              ./hosts/node-01
              ./hosts/modules/home-manager.nix
              ./hosts/modules/impermanence
              ./hosts/modules/impermanence/backup.nix
              ./hosts/modules/locale.nix
              ./hosts/modules/monitoring.nix
              ./hosts/modules/networking
              ./hosts/modules/nginx.nix
              ./hosts/modules/nix
              ./hosts/modules/packages.nix
              ./hosts/modules/system
              ./hosts/modules/system/efi.nix
              ./hosts/modules/system/users
              home-manager.nixosModules.home-manager
              sops-nix.nixosModules.sops
            ] ++ [{
              networking.hostName = "node-01";
              time.timeZone = "Europe/Paris";
              system.stateVersion = "22.11";

              fileSystems."/boot".device = "/dev/disk/by-uuid/bde2fa2e-dd10-4a8c-8c1b-2993b8b8b8d3";
              fileSystems."/boot/efi".device = "/dev/disk/by-uuid/5206-7A9C";
              boot.initrd.luks.devices.root.device = "/dev/disk/by-uuid/7cfc7623-2ee6-4122-8faf-483f3db15264";
              boot.initrd.network.ssh.hostKeys = [ "${mySystemDirectory}/etc/ssh/ssh_initrd_ed25519_key" ];

              home-manager = {
                extraSpecialArgs.myLib = import ./homes/lib { inherit (nixpkgs) lib; };
                users =
                  let
                    minimal = { ... }: {
                      imports = [ ./homes/minimal.nix ];
                      home.stateVersion = "22.11";
                    };
                  in
                    {
                      root = minimal;
                      ether = minimal;
                    };
              };
            }];
          };
        };
  };
}
