let
  pkgs = import <nixpkgs> {};
in
pkgs.buildGoModule rec {
  pname = "cfssl";
  version = "dac37af48dd94ab50fbdc182d7ec0d634520607b";

  src = pkgs.fetchFromGitHub {
    owner = "cloudflare";
    repo = "cfssl";
    rev = "${version}";
    sha256 = "sha256-a4b4ixticbgUirht2mnRj9uil7sHsqhYZGYf0+QAYnc=";
  };

  subPackages = [
    "cmd/cfssl"
    "cmd/cfssljson"
    "cmd/cfssl-bundle"
    "cmd/cfssl-certinfo"
    "cmd/cfssl-newkey"
    "cmd/cfssl-scan"
    "cmd/multirootca"
    "cmd/mkbundle"
  ];

  vendorHash = null;

  doCheck = false;

  ldflags = [
    "-s" "-w"
    "-X github.com/cloudflare/cfssl/cli/version.version=v${version}"
  ];
}
