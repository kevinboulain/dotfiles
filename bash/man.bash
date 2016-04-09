export MANPATH=$(man --path)

# add the passed directory to $MANPATH if it exists
function safe_prepend_to_manpath {
    if [ $# -ne 1 ]; then
        log "needs one parameter"
        return 1
    fi

    local -r directory=$1
    if [ -d "$directory" ]; then
        prepend_to_var MANPATH ':' "$directory"
    else
        log "'$directory' not found"
    fi
}
