# https://github.com/NixOS/nixpkgs/issues/31294#issuecomment-457045667
self: super: {
  iosevka-term = super.iosevka.override {
    set = "term";
  };
}
