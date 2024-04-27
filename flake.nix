{
  inputs = {
    home-manager = {
      url = "github:nix-community/home-manager";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    sin = {
      url = "github:ether42/sin";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    sops-nix = {
      url = "github:mic92/sops-nix";
      inputs.nixpkgs.follows = "nixpkgs";
      # Their nixpkgs-stable doesn't appear to be used outside tests, one less
      # dependency.
      inputs.nixpkgs-stable.follows = "nixpkgs";
    };
    swaybar = {
      url = "github:ether42/swaybar";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = { home-manager, nixpkgs, self, sin, sops-nix, swaybar, ... }: {
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
          inherit myPublicKey myStateDirectory mySystemDirectory nixpkgs self;
          myLib = import ./lib { inherit (nixpkgs) lib; };
          myHostsLib = import ./hosts/lib { inherit (nixpkgs) lib; inherit mySystemDirectory; };
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
              ./hosts/modules/networking/mullvad.nix
              ./hosts/modules/nginx
              ./hosts/modules/nginx/static.nix
              ./hosts/modules/nix
              ./hosts/modules/nix/ccache.nix
              ./hosts/modules/nix/extras.nix
              ./hosts/modules/packages
              ./hosts/modules/packages/extras.nix
              ./hosts/modules/rescue.nix
              ./hosts/modules/steam.nix
              ./hosts/modules/system
              ./hosts/modules/system/efi.nix
              ./hosts/modules/system/users
              ./hosts/modules/system/users/ether.nix
              ./hosts/modules/system/users/untrusted.nix
              ./hosts/modules/usbguard.nix
              ./hosts/modules/virtualization/qemu.nix
              ./hosts/modules/yubikey.nix
              home-manager.nixosModules.home-manager
              sops-nix.nixosModules.sops
            ] ++ [{
              networking.hostName = "acheron";
              time.timeZone = "Europe/Paris";
              system.stateVersion = "22.11";

              # To ease testing for some of the AArch64 hosts below.
              boot.binfmt.emulatedSystems = [ "aarch64-linux" ];

              fileSystems."/boot/efi".device = "/dev/disk/by-uuid/87AC-02CE";
              fileSystems."/boot/rescue".device = "/dev/disk/by-uuid/96b89522-deab-42d3-ab43-0040bbb0e47b";
              boot.kernelParams = [
                # Run 'btrfs inspect-internal map-swapfile path/to/swap' to find out.
                # A full rebalance might break it:
                # https://bugzilla.kernel.org/show_bug.cgi?id=217066
                "resume_offset=26486016"
              ];
              boot.initrd.luks.devices.root.device = "/dev/disk/by-uuid/a8cb3a4c-3b49-44db-a476-fc02551063b3";

              home-manager = {
                extraSpecialArgs = {
                  inherit sin swaybar;
                  myHomesLib = import ./homes/lib { inherit (nixpkgs) lib; };
                };
                users =
                  let
                    home.stateVersion = "22.11";
                    wayland.windowManager.sway.hiDPIFix = true;
                  in
                    {
                      ether = { ... }: {
                        inherit home wayland;
                        imports = [
                          ./homes/desktop.nix
                          ./homes/modules/emacs/notmuch.nix
                          ./homes/modules/yubikey.nix
                        ];
                      };
                      root = { ... }: {
                        inherit home;
                        imports = [ ./homes/minimal.nix ];
                      };
                      untrusted = { ... }: {
                        inherit home wayland;
                        imports = [ ./homes/desktop.nix ];
                      };
                    };
              };
            }];
          };

          node-02 = nixpkgs.lib.nixosSystem {
            inherit specialArgs;
            system = "aarch64-linux";
            modules = [
              ./hosts/node-02
              ./hosts/modules/acme.nix
              ./hosts/modules/home-manager.nix
              ./hosts/modules/impermanence
              ./hosts/modules/impermanence/backup.nix
              ./hosts/modules/locale.nix
              ./hosts/modules/matrix
              ./hosts/modules/matrix/root.nix
              ./hosts/modules/monitoring.nix
              ./hosts/modules/networking
              ./hosts/modules/nginx
              ./hosts/modules/nginx/root.nix
              ./hosts/modules/nginx/static.nix
              ./hosts/modules/nix
              ./hosts/modules/packages
              ./hosts/modules/system
              ./hosts/modules/system/efi.nix
              ./hosts/modules/system/users
              ./hosts/modules/system/users/ether.nix
              home-manager.nixosModules.home-manager
              sops-nix.nixosModules.sops
            ] ++ [rec {
              networking.hostName = "node-02";
              time.timeZone = "Europe/Paris";
              system.stateVersion = "23.05";

              system.autoUpgrade = {
                enable = true;
                flake = "github:kevinboulain/dotfiles#${networking.hostName}";
                flags = [
                  "--refresh"  # https://github.com/NixOS/nix/issues/4007
                ];
                operation = "switch";
                allowReboot = false;
              };

              security.acme.defaults.email = "admin+acme@boula.in";
              services.nginx.virtualHosts = {
                root.serverName = "boula.in";
                element.serverName = "element.boula.in";
                matrix.serverName = "matrix.boula.in";
                static = {
                  serverName = "static.boula.in";
                  enableACME = true;
                  forceSSL = true;
                };
              };

              fileSystems."/boot".device = "/dev/disk/by-uuid/89733135-b593-4106-9801-480900e0facb";
              fileSystems."/boot/efi".device = "/dev/disk/by-uuid/2C92-557F";
              boot.initrd.luks.devices.root.device = "/dev/disk/by-uuid/c3427944-91ea-4314-8844-462850458ce5";

              home-manager = {
                extraSpecialArgs.myHomesLib = import ./homes/lib { inherit (nixpkgs) lib; };
                users =
                  let
                    minimal = { ... }: {
                      imports = [ ./homes/minimal.nix ];
                      home.stateVersion = "23.05";
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

    packages.x86_64-linux =
      let
        myLib = import ./lib { inherit (nixpkgs) lib; };
        pkgs = import nixpkgs {
          system = "x86_64-linux";
          config.allowUnfreePredicate = myLib.allowUnfreePredicate [ "steam-original" "steam-run" ];
        };
      in
        {
          ngfx-ui-wrapper = pkgs.callPackage ./packages/ngfx-ui-wrapper.nix {};
        };
  };
}
