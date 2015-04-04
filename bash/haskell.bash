# cabal default directory
cabal=~/.cabal
if [ -d "$cabal" ]; then
    export PATH="$cabal/bin:$PATH"
fi
unset cabal

# return whether or not we are in a cabal sandbox
function sandboxed {
    local -r cabal_sandbox_config="cabal.sandbox.config"
    [ -f "$cabal_sandbox_config" ]
}
