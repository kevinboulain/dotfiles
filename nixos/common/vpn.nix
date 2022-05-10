{ pkgs, ... }:
{
  # TODO: all users can access account details.
  services.mullvad-vpn.enable = true;
  environment.systemPackages = with pkgs; [
    mullvad  # CLI client.
  ];
}
