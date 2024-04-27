# Get the .deb at https://developer.nvidia.com/nsight-graphics and extract it
# somewhere. Not pretty but it does the job... Tested with
# NVIDIA_Nsight_Graphics_2024.1.0.24079.deb.
{
  lib,
  krb5,
  steam-run,
  writeScriptBin,
  xcb-util-cursor,
  xorg,
}:
writeScriptBin "ngfx-ui-wrapper" ''
  ngfx_ui=''${1?}
  directory=$(dirname "''$ngfx_ui")
  export LD_LIBRARY_PATH="''$LD_LIBRARY_PATH":"''$directory":${
    lib.makeLibraryPath [
      krb5 # libgssapi_krb5.so.2
      xcb-util-cursor # libxcb-cursor.so.0
      xorg.xcbutilimage # libxcb-image.so.0
      xorg.xcbutilkeysyms # libxcb-keysyms.so.1
      xorg.xcbutilrenderutil # libxcb-render-util.so.0
      xorg.xcbutilwm # libxcb-icccm.so.4
    ]
  }
  # Could not find the Qt platform plugin "wayland" in ""
  # Could not find the Qt platform plugin "xcb" in ""
  #  - Qt plugin search path: .../opt/nvidia/nsight-graphics-for-linux/nsight-graphics-for-linux-2024.1.0.0/host/linux-desktop-nomad-x64
  export QT_PLUGIN_PATH="''$directory"/Plugins
  "${steam-run}"/bin/steam-run "''$@"
''
