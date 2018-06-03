order_path() {
  local -a path
  IFS=: read -r -a paths <<< "$PATH"
  local -A unique_paths
  PATH=
  local path
  # standard locations comes after any user-defined $PATH
  for path in ~/.cargo/bin /{,usr/{,local/}}{bin,sbin} "${paths[@]}"; do
    # if path exists and if it isn't already in path, prepend it
    if [ -d "$path" ] && [ -z "${unique_paths[$path]+_}" ]; then
      unique_paths[$path]=_
      PATH=$path${PATH+:$PATH}
    fi
  done
  export PATH
}
declare -rfx order_path

order_path
