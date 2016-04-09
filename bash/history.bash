# ignore duplicates in history
export HISTCONTROL=ignoredups:erasedups

# append to history, do not overwrite it
shopt -s histappend

# specific history for each spawned shell
history_directory=~/.bash_history.d
export HISTFILE=$history_directory/$(date -u +%Y/%m/%d)/$(date -u +%H:%M:%S)_$(hostname)_$$

# remove history limitations
unset HISTSIZE HISTFILESIZE

function make_history_directory {
    local -r history_subdirectory=$(dirname "$HISTFILE")
    if [ ! -d "$history_subdirectory" ]; then
        mkdir -p "$history_subdirectory"
    fi
}

# append each command to the history file
append_prompt_command make_history_directory
append_prompt_command 'history -a'

function hist {
    if hash ag >& "$null"; then
        ag "$@" "$history_directory"
    else
        grep -r "$@" "$history_directory"
    fi
}
