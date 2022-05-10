self: super: {
  sof-firmware = super.stdenvNoCC.mkDerivation rec {
    pname = "sof-firmware";
    version = "2.0";

    src = super.fetchFromGitHub {
      owner = "thesofproject";
      repo = "sof-bin";
      rev = "v${version}";
      sha256 = "sha256-pDxNcDe/l1foFYuHB0w3YZidKIeH6h0IuwRmMzeMteE=";
    };

    dontFixup = true; # binaries must not be stripped or patchelfed

    installPhase = ''
      runHook preInstall
      cd "v${super.lib.versions.majorMinor version}.x"
      mkdir -p $out/lib/firmware/intel/
      cp -a sof-v${version} $out/lib/firmware/intel/sof
      cp -a sof-tplg-v${version} $out/lib/firmware/intel/sof-tplg
      runHook postInstall
    '';
  };
}
