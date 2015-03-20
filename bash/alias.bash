#!/usr/bin/env bash

# if the command (with or without parameters) passed as second paremeter exists
# make first parameter an alias of it
# not used
function alias_existing_command {
    if [ $# -ne 2 ] || [ -z "$1" ] || [ -z "$2" ]; then
        log "needs two parameters"
        return 1
    fi
    local -r alias=$1
    # set will set every positional parameter to $1..$x and $# to x
    set -- $2
    local -r to_alias=$1
    hash "$to_alias" >& "$null"
    if [ $? -eq 0 ]; then
        # ${@:x} slice from x
        eval "alias $alias=\"\$to_alias \${@:2}\""
        return 0
    fi
    # no need to signal an error for now
    #log "$1: not found"
    return 1
}
