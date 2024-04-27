{ myHostsLib, ... }:
let
  inherit (myHostsLib) state;
in
{
  fileSystems = state.binds [
    # Stores challenges, certificates and state.
    "/var/lib/acme"
  ];

  # Done here so it's impossible to forget to include this module.
  security.acme.acceptTerms = true;
}
