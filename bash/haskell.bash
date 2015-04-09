# cabal default directory
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

# check local sandbox too
export PATH="./.cabal-sandbox/bin:$PATH"
