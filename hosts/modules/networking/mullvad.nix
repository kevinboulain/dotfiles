{ config, lib, myHostsLib, ... }:
let
  inherit (myHostsLib) state;
in
{
  services.mullvad-vpn.enable = true;
  systemd.services.mullvad-daemon.environment = {
    # https://github.com/mullvad/mullvadvpn-app/#environment-variables-used-by-the-service
    MULLVAD_MANAGEMENT_SOCKET_GROUP = "wheel";
  };
  # Stores the account number.
  fileSystems = state.binds (lib.optional config.services.mullvad-vpn.enable "/etc/mullvad-vpn");
}
