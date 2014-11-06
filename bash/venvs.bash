#!/usr/bin/env bash

# require virtualenvwrapper (install it via pip)

# deactivate the current venv, if any
function deactivate_venv {
    if [ -n "$VIRTUAL_ENV" ]; then
        # deactivate only if working on a venv
        log "currently using venv '$VIRTUAL_ENV', deactivating it."
        deactivate
    fi
}

# function to use a default python venv, may create it
# needs a single parameter which is where are stored the venvs
function activate_default_venv {
    if [ $# -ne 1 ] || [ ! -n "$1" ]; then
        log "needs the path to python venvs as a parameter"
        return 1
    fi

    # where the vens are stored
    local config_directory=$1
    # virtualenvwrapper.sh path
    local virtualenvwrapper="`which virtualenvwrapper.sh`"

    # test if $config_directory exists
    if [ ! -d "$config_directory" ]; then
        log "'$config_directory' directory not found."
        return 1
    fi

    # test if virtualenvwrapper is installed
    if [ ! -f "$virtualenvwrapper" ]; then
        log "'virtualenvwrapper.sh' script not found."
        return 1
    fi

    deactivate_venv

    # where to store the venvs
    local python_directory="$config_directory/python"
    WORKON_HOME="$python_directory/venv"

    # test if $WORKON_HOME directory exists
    if [ ! -d "$WORKON_HOME" ]; then
        # create it
        log "'$WORKON_HOME' directory not found, creating it."
        mkdir -p "$WORKON_HOME" >& "$null"
        if [ $? -ne 0 ]; then
            log "'$WORKON_HOME' directory could not be made."
            unset WORKON_HOME
            return 1
        fi
    fi
    export PYTHONPATH="$python_directory"

    # source virtualenvwrapper.sh
    export WORKON_HOME
    . "$virtualenvwrapper"

    # this function creates and uses $default_venv
    default_venv='stuff'

    # test if $default_venv exists
    lsvirtualenv -b 2> "$null" | grep "$default_venv" >& "$null"
    if [ $? -eq 0 ]; then
        # use it
        workon "$default_venv"
    else
        # create it
        log "'$default_venv' venv not found, creating it."
        mkvirtualenv "$default_venv"
    fi

    # everything is okay
    return 0
}
