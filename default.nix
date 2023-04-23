# For nixos-option: https://github.com/NixOS/nixpkgs/issues/97855
#  configuration="((import ~/sources/dotfiles).nixosConfigurations.${HOSTNAME?})" && nixos-option --config_expr "$configuration.config" --options_expr "$configuration.options"
(import (fetchTarball "https://github.com/edolstra/flake-compat/archive/master.tar.gz") {
  src = ./.;
}).defaultNix
