IFS=: read -r -a paths <<< "$PATH"
declare -A unique_paths
PATH=
# standard locations comes after any user-defined $PATH
for path in /{,usr/{,local/}}{bin,sbin} "${paths[@]}"; do
    # if path exists and if it isn't already in path, prepend it
    if [ -d "$path" ] && [ -z "${unique_paths[$path]+_}" ]; then
        unique_paths[$path]=_
        PATH=$path${PATH:+:$PATH}
    fi
done
unset unique_paths paths path
export PATH
