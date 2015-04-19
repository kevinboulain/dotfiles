# cabal default directory
# should not be used, sandboxes are less problematic
cabal=~/.cabal
if [ -d "$cabal" ]; then
    export PATH="$cabal/bin:$PATH"
fi
unset cabal

# return whether or not the actual directory has a cabal sandbox configuration
function sandboxed {
    local -r cabal_sandbox_config="cabal.sandbox.config"
    [ -f "$cabal_sandbox_config" ]
}

# test if the current shell is sandboxed
function current_sandbox {
    if [ -n "$CABAL_SANDBOX_CONFIG" ]; then
        echo "$CABAL_SANDBOX_CONFIG"
        return 0
    fi
    return 1
}

# function to use a default haskell sandbox, may create it
function activate_default_sandbox {
    if [ $# -ne 1 ] || [ -z "$1" ]; then
        log "needs the path to Haskell sandbox as a parameter"
        return 1
    fi
    # where the sandbox is stored
    local -r config_directory=$1
    # test if $config_directory exists
    if [ ! -d "$config_directory" ]; then
        log "'$config_directory' directory not found, will not setup Haskell..."
        return 1
    fi

    # our haskell directory
    local -r haskell_directory="$config_directory/haskell"
    # test if $haskell_directory directory exists
    if [ ! -d "$haskell_directory" ]; then
        # create it
        log "'$haskell_directory' directory not found, creating it."
        mkdir -p "$haskell_directory" >& "$null"
        if [ $? -ne 0 ]; then
            log "'$haskell_directory' directory could not be made."
            unset haskell_directory
            return 1
        fi
    fi

    # test if $haskell_directory is not already sandboxed
    if ! (cd "$haskell_directory" && sandboxed); then
        # if cabal does not exist abort
        if ! hash cabal >& "$null"; then
            log "'cabal' command not found, could not create a default sandbox."
            return 1
        fi

        # there isn't a sandbox here, create one
        log "default sandbox in '$haskell_directory' not found, creating it."
        (cd "$haskell_directory" && cabal sandbox init) || return 1
    else
        #log "default sandbox in '$haskell_directory' found, using it."
        :
    fi

    # add the default sandbox to the PATH
    export PATH="$haskell_directory/.cabal-sandbox/bin:$PATH"
    # check local sandboxes too
    export PATH="./.cabal-sandbox/bin:$PATH"
}
