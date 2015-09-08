#!/usr/bin/env bash

# prepend a variable to another variable
# note that the variable is exported
function prepend_to_var {
    if [ $# -ne 3 ] || [ -z "$1" ] || [ -z "$2" ] || [ -z "$3" ]; then
        log "needs three parameters"
        return 1
    fi

    local -r var=$1
    local -r separator=$2
    local -r value=$3
    if [ -z "${!var}" ]; then
        export "$var=$value"
    else
        export "$var=$value$separator${!var}"
    fi
}

# append a variable to another variable
# note that the variable is exported
function append_to_var {
    if [ $# -ne 3 ] || [ -z "$1" ] || [ -z "$2" ] || [ -z "$3" ]; then
        log "needs three parameters"
        return 1
    fi

    local -r var=$1
    local -r separator=$2
    local -r value=$3
    if [ -z "${!var}" ]; then
        export "$var=$value"
    else
        export "$var=${!var}$separator$value"
    fi
}
