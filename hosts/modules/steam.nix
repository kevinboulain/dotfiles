{ pkgs, ... }: {
  nixpkgs.config.allowUnfree = true;  # Steam.
  programs = {
    steam.enable = true;  # Installing via nix-env doesn't work: "libnvidia-glvkspirv.so is required at runtime".
    gamemode.enable = true;  # For performance-sensitive applications.
  };
  hardware.opengl.driSupport32Bit = true;
  environment.systemPackages = with pkgs; [
    mangohud
  ];
}
