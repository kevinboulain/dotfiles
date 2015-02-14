# cabal default directory
cabal=~/.cabal
if [ -d "$cabal" ]; then
    export PATH="$cabal/bin:$PATH"
fi
unset cabal
