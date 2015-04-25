#!/usr/bin/env bash

# require virtualenvwrapper (install it via pip)

# disable the custom prompt setup by virtualenv
export VIRTUAL_ENV_DISABLE_PROMPT=1

# source virtualenvwrapper script
function source_script_venv {
    if [ -z "$WORKON_HOME" ]; then
        log "'\$WORKON_HOME' variable needs to be set..."
        return 1
    fi

    # virtualenvwrapper.sh path
    local -r virtualenvwrapper=$(which virtualenvwrapper.sh)
    # test if virtualenvwrapper is installed
    if [ ! -f "$virtualenvwrapper" ]; then
        log "'virtualenvwrapper.sh' script was not found."
        return 1
    fi

    # source it
    . "$virtualenvwrapper" && return 0

    # something failed
    log "could not source 'virtualenvwrapper.sh' script..."
    return 1
}

# echo to stdout the current venv
function current_venv {
    # if a venv is set
    if [ -n "$VIRTUAL_ENV" ]; then
        # get the directory name from its path
        basename "$VIRTUAL_ENV"
        return 0
    fi
    return 1
}

# deactivate the current venv, if any
function deactivate_current_venv {
    # deactivate only if working in a venv
    if [ -n "$VIRTUAL_ENV" ] && [ -n "$WORKON_HOME" ]; then
        # get the current venv name
        local -r venv=$(current_venv)

        # if we do not have the deactivate function
        if [ "$(type -t deactivate)" != 'function' ]; then
            # source the virtualenvwrapper script to continue deactivation
            source_script_venv
            # something failed, source_script_venv() already log failures
            [ $? -ne 0 ] && return 1

            # re-work on the venv to be able to deactivate it
            workon "$venv"
            if [ $? -ne 0 ]; then
                log "asked to deactivate current venv '$venv',"\
                    "but deactivate() function was not found"\
                    "and the venv could not be reloaded..."
                return 1
            fi
        fi

        # deactivate it, too verbose over time
        #log "currently using venv '$venv', deactivating it."
        deactivate && return 0
    else
        # no need to signal an error for now
        #log "'\$WORKON_HOME' and '\$VIRTUAL_ENV' need to be set"
        : # noop
    fi
    return 1
}

# function to use a default python venv, may create it
# PATH modification should take before using the venv
function activate_default_venv {
    if [ $# -ne 1 ] || [ -z "$1" ]; then
        log "needs the path to Python venvs as a parameter"
        return 1
    fi

    # where the vens are stored
    local -r config_directory=$1
    # test if $config_directory exists
    if [ ! -d "$config_directory" ]; then
        log "'$config_directory' directory not found, will not setup Python..."
        return 1
    fi

    # deactivate the current venv, if any, no need to check the return code
    deactivate_current_venv

    # our python directory
    local -r python_directory=$config_directory/python
    # where to store the venvs
    WORKON_HOME=$python_directory/venvs
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

    # we may have some python scripts here
    export PYTHONPATH=$python_directory

    # source virtualenvwrapper.sh
    export WORKON_HOME
    source_script_venv
    # something failed, source_script_venv() already log failures
    [ $? -ne 0 ] && return 1

    # this function creates and uses $default_venv
    default_venv=stuff
    # test if $default_venv exists
    lsvirtualenv -b 2> "$null" | grep "$default_venv" >& "$null"
    if [ $? -eq 0 ]; then
        # use it
        #log "default venv '$default_venv' found, using it."
        workon "$default_venv" && return 0
    else
        # create it
        log "default venv '$default_venv' not found, creating it."
        mkvirtualenv "$default_venv" && return 0
    fi

    # something failed
    log "could not load default venv '$default_venv'..."
    return 1
}
