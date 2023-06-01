{ lib, mySystemDirectory, ... }: {
  imports = [ ./system.nix ];

  nixpkgs.overlays = [
    (final: parent: {
      # Don't ask why I can't use Python's packageOverrides instead... And yes,
      # the service doesn't allow overriding the package so an overlay is
      # necessary.
      octoprint = parent.octoprint.override {
        packageOverrides = final: parent:
          let
            skipShpinx = package: (package.override {
              sphinxHook = null;
            }).overridePythonAttrs (old: {
              outputs = lib.lists.remove "doc" old.outputs;
            });
          in
            {
              argon2-cffi-bindings = parent.argon2-cffi-bindings.overrideAttrs (old: {
                ARGON2_CFFI_USE_SSE2 = 0;
                # https://github.com/NixOS/nixpkgs/pull/221951
                nativeBuildInputs = old.nativeBuildInputs ++ old.propagatedBuildInputs;
              });
              # No clue, but works around:
              #  .../sphinx-build: line 3: syntax error near unexpected token `lambda'
              deprecated = skipShpinx parent.deprecated;
              wrapt = skipShpinx parent.wrapt;
            };
      };
    })
  ];

  services.octoprint = {
    enable = true;
    openFirewall = true;
    stateDir = "${mySystemDirectory}/var/lib/octoprint";
    plugins = plugins: with plugins; [
      bedlevelvisualizer
      simpleemergencystop
      themeify
      # https://github.com/jneilliii/OctoPrint-ipOnConnect would be nice but
      # mDNS kinda covers that too.
    ];
  };
}
