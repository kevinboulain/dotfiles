# ignore duplicates in history
HISTCONTROL=ignoredups:erasedups

# append to history, do not overwrite it
shopt -s histappend

# specific history for each spawned shell
history_directory=~/.bash_history.d
HISTFILE=$history_directory/$(date -u +%Y/%m/%d)/$(date -u +%H:%M:%S)_$(hostname)_$$

# remove history limitations
unset HISTSIZE HISTFILESIZE

# append each command to its corresponding history file
history_subdirectory=$(dirname "$HISTFILE")
[ -d "$history_subdirectory" ] || mkdir -p "$history_subdirectory"
unset history_subdirectory
PROMPT_COMMAND=${PROMPT_COMMAND:+$PROMPT_COMMAND;}'history -a'

function hist {
    if hash ag >& "$null"; then
        ag "$@" "$history_directory"
    else
        grep -r "$@" "$history_directory"
    fi
}