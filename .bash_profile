# some exports
export PATH='/usr/local/bin:/usr/bin:/bin:/usr/sbin:/sbin'
export EDITOR='emacs'
export PAGER='less'

# some escape codes
cyan="\033[36m"
red="\033[31m"
esc="\033[0m"

# executed at each command, set $prompt_color
PROMPT_COMMAND='
if [ $? -eq 0 ]; then
     prompt_color=$cyan
else
     prompt_color=$red
fi'
# prompt, \[\]: allow readline to correctly calculate prompt size
PS1='\h:\W \[$(echo -ne $prompt_color)\]>\[$(echo -ne $esc)\] '

# handy variables
null='/dev/null'

# a simple function that log the caller name and all its parameter
function log {
    local function_name='cli'
    if [ -n "${FUNCNAME[1]}" ]; then
        function_name=${FUNCNAME[1]}
    fi
    function_name="$function_name()"

    if [ $# -eq 0 ]; then
        echo "$function_name: called ${FUNCNAME[0]} without any message"
    else
        echo "$function_name: $@"
    fi
}

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
    local venvs_directory=$1
    # virtualenvwrapper.sh path
    local virtualenvwrapper="`which virtualenvwrapper.sh`"

    # test if $venvs_directory exists
    if [ ! -d "$venvs_directory" ]; then
        log "'$venvs_directory' directory not found."
        return 1
    fi

    # test if virtualenvwrapper is installed
    if [ ! -f "$virtualenvwrapper" ]; then
        log "'virtualenvwrapper.sh' script not found."
        return 1
    fi

    deactivate_venv

    # where to store the venvs
    WORKON_HOME="$venvs_directory/stuff/python/venv"

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

# some Mac OS X specific stuff
if [ "`uname`" = 'Darwin' ]; then
    # where some stuff should be stored
    data_mount='/Volumes/Data'

    # test if $data_mount if correctly mounted
    mount 2> "$null" | grep "$data_mount (hfs, local, journaled)" >& "$null"
    if [ $? -eq 0 ]; then
        # activate the default python venv to not mess with the system python
        activate_default_venv "$data_mount"

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
fi

alias emacs='emacs -nw'
alias clean="find . -name '*.pyc' -delete -or -name '*~' -delete -or -name '*.o' -delete"
alias clean_list="find . -name '*.pyc' -or -name '*~' -or -name '*.o'"
alias l='ls'
alias lsvirtualenv='lsvirtualenv -b'
