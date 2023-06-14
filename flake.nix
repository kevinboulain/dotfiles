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
              ./hosts/modules/networking.nix
              ./hosts/modules/nix
              ./hosts/modules/packages.nix
              ./hosts/modules/rescue.nix
              ./hosts/modules/steam.nix
              ./hosts/modules/system.nix
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
              boot.rescue.isos = [ "nixos-minimal-23.05pre461993.0c4800d579a-x86_64-linux.iso" ];
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
                          ./homes/modules/gdb
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

          cocytus = nixpkgs.lib.nixosSystem {
            inherit specialArgs;
            system = "x86_64-linux";
            modules = [
              ./hosts/cocytus
              ./hosts/modules/home-manager.nix
              ./hosts/modules/impermanence
              ./hosts/modules/impermanence/backup.nix
              ./hosts/modules/locale.nix
              ./hosts/modules/monitoring.nix
              ./hosts/modules/networking.nix
              ./hosts/modules/nginx.nix
              ./hosts/modules/nix
              ./hosts/modules/packages.nix
              ./hosts/modules/system.nix
              home-manager.nixosModules.home-manager
              sops-nix.nixosModules.sops
            ] ++ [{
              networking.hostName = "cocytus";
              time.timeZone = "Europe/Paris";
              system.stateVersion = "22.11";

              fileSystems."/boot".device = "/dev/disk/by-uuid/a6068654-68d4-4389-8197-becf0f9ecec8";
              fileSystems."/boot/efi".device = "/dev/disk/by-uuid/D43D-D268";
              boot.initrd.luks.devices.root.device = "/dev/disk/by-uuid/3e0e069f-87c3-474c-9aee-b2aa1239a0a4";
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
                      untrusted = minimal;
                    };
              };
            }];
          };
        };
  };
}
