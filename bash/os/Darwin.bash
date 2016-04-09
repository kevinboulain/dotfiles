# some Mac OS X specific stuff

# where some stuff should be stored
data_mount=/Volumes/Data
data_disk=/dev/disk2

# coreutils via brew
# findutils (xargs) via brew, need --with-default-names
safe_prepend_to_path /usr/local/opt/coreutils/libexec/gnubin
safe_prepend_to_manpath /usr/local/opt/coreutils/libexec/gnuman

# sed via brew
safe_prepend_to_path /usr/local/opt/gnu-sed/libexec/gnubin
safe_prepend_to_manpath /usr/local/opt/gnu-sed/libexec/gnuman

# cross compiler binaries
safe_prepend_to_path "$data_mount/documents/cross-compilers/bin"
safe_prepend_to_manpath "$data_mount/documents/cross-compilers/share/man"

# $HOMEBREW_TEMP must be set if Homebrew directory's is on a separate partition
HOMEBREW_TEMP=$data_mount/tmp
if [ -d "$HOMEBREW_TEMP" ]; then
    export HOMEBREW_TEMP
else
    unset HOMEBREW_TEMP
fi

# test if $data_mount if correctly mounted
mount 2> "$null" |
    grep "^$data_disk on $data_mount (hfs, local, journaled)$" >& "$null"
if [ $? -eq 0 ]; then
    # activate the default python venv to not mess with the system python
    activate_default_venv "$config_directory"
    activate_default_sandbox "$config_directory"

    # if brew is a file, overwrite it with this function to handle venvs
    if [ $? -eq 0 ] && [ "$(type -t brew)" = 'file' ]; then
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
