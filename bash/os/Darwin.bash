#!/usr/bin/env bash

# some Mac OS X specific stuff

# where some stuff should be stored
data_mount='/Volumes/Data'

# test if $data_mount if correctly mounted
mount 2> "$null" | grep "$data_mount (hfs, local, journaled)" >& "$null"
if [ $? -eq 0 ]; then
    # activate the default python venv to not mess with the system python
    activate_default_venv "$config_directory"

    # if brew is a file, overwrite it with this function to handle venvs
    if [ $? -eq 0 ] && [ "`type -t brew`" = 'file' ]; then
        function brew {
            # brew can not work in a venv
            deactivate_venv
            /usr/local/bin/brew "$@"
        }
    fi
else
    log "$data_mount is not properly mounted."
fi

# indicate to Finder whether it should show all files or not
function finder_show_all {
    if [ "$1" == 'true' ] || [ "$1" == 'false' ]; then
        /usr/bin/defaults write com.apple.finder AppleShowAllFiles $1
        /usr/bin/killall Finder
    else
        log 'boolean parameter required.'
    fi
}

# oh hai lytchi :)
alias lssh='ssh -l root -p 4222 -i ~/.lytchi_ssh/root'
