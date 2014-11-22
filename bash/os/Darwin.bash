#!/usr/bin/env bash

# some Mac OS X specific stuff

# where some stuff should be stored
data_mount='/Volumes/Data'
data_disk='/dev/disk2'

# test if $data_mount if correctly mounted
mount 2> "$null" |
    grep "^$data_disk on $data_mount (hfs, local, journaled)$" >& "$null"
if [ $? -eq 0 ]; then
    # activate the default python venv to not mess with the system python
    activate_default_venv "$config_directory"

    # if brew is a file, overwrite it with this function to handle venvs
    if [ $? -eq 0 ] && [ "`type -t brew`" = 'file' ]; then
        # brew can not work in a venv
        function brew {
            deactivate_current_venv
            /usr/local/bin/brew "$@"
        }
    fi
else
    log "$data_mount is not properly mounted on $data_disk."
fi

# indicate to Finder whether it should show all files or not
function finder_show_all {
    if [ "$1" == 'true' ] || [ "$1" == 'false' ]; then
        /usr/bin/defaults write com.apple.finder AppleShowAllFiles $1
        /usr/bin/killall Finder
    else
        log "boolean parameter required: 'true' or 'false'."
    fi
}

# coreutils via brew
coreutils='/usr/local/opt/coreutils'
if [ -d "$coreutils" ]; then
    export PATH="$coreutils/libexec/gnubin:$PATH"
    export MANPATH="$coreutils/libexec/gnuman:$MANPATH"
fi
unset coreutils

# cross compiler binaries
cross='/Volumes/Data/documents/cross-compilers'
if [ -d "$cross" ]; then
    export PATH="$cross/bin:$PATH"
    export MANPATH="$cross/share/man:$MANPATH"
fi
unset cross
