{ lib, ... }: rec {
  # Similar to lib.filesystem.listFilesRecursive but never resolves the paths.
  # :p (listFilesAsStringRecursive ./. [ "homes/modules/sway/sway" ])
  # [ "homes/modules/sway/sway/config" "homes/modules/sway/sway/config.d/idle" ... ]
  listRelativeFilesRecursive = root: paths:
    lib.flatten (map (path:
      lib.mapAttrsToList (name: type:
        if type == "directory" then listRelativeFilesRecursive root [ "${path}/${name}" ]
        else "${path}/${name}"
      ) (builtins.readDir (root + "/${path}"))
    ) paths);

  # :p (copyTrees ./. [ "homes/modules/sway/sway" ])
  # { "homes/modules/sway/sway/config" = { source = .../homes/modules/sway/sway/config; };
  #   "homes/modules/sway/sway/config.d/idle" = { source = .../homes/modules/sway/sway/config.d/idle; };
  #   ... }
  copyTrees = root: paths:
    builtins.listToAttrs (map (path: {
      name = path;
      value = { source = root + "/${path}"; };
    }) (listRelativeFilesRecursive root paths));
}
