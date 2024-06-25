{
  config,
  lib,
  pkgs,
  ...
}:
{
  programs.sway = {
    extraOptions = [ "--unsupported-gpu" ];
    extraSessionCommands = ''
      # Color profiles only take effect under Vulkan.
      export WLR_RENDERER=vulkan
    '';
  };
  environment.etc =
      {
        "sway/config.d/output".source =
          assert config.programs.sway.enable;
          pkgs.writeText "output" ''
            # The profile was retrieved from the original Windows installation.
            output eDP-1 color_profile icc ${./profile.icm}
          '';
      };
}
