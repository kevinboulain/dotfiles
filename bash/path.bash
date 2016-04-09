export PATH='/usr/local/bin:/usr/bin:/bin:/usr/local/sbin:/usr/sbin:/sbin'

# add the passed directory to $PATH
function prepend_to_path {
    if [ $# -ne 1 ] || [ -z "$1" ]; then
        log "needs one parameter"
        return 1
    fi

    local -r directory=$1
    prepend_to_var PATH ':' "$directory"
}

# add the passed directory to $PATH if it exists
function safe_prepend_to_path {
    if [ $# -ne 1 ]; then
        log "needs one parameter"
        return 1
    fi

    local -r directory=$1
    if [ -d "$directory" ]; then
        prepend_to_path "$directory"
    else
        log "'$directory' not found"
    fi
}
