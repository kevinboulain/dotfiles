HISTCONTROL=ignoredups # ignore duplicates in history
HISTCONTROL+=:erasedups # remove duplicates from history
HISTCONTROL+=:ignorespace # don't store lines beginning with spaces in history

# date format for 'history'
HISTTIMEFORMAT='%F %T '

# append to history, do not overwrite it
shopt -s histappend

# specific history for each spawned shell
declare -r history_directory=~/.bash_history.d
HISTFILE=$history_directory/$(date -u +%Y/%m/%d)/$(date -u +%H:%M:%S)_$(hostname)_$$

# remove history limitations
HISTSIZE=-1
HISTFILESIZE=-1

# append each command to its corresponding history file
history_subdirectory=$(dirname "$HISTFILE")
[ -d "$history_subdirectory" ] || mkdir -p "$history_subdirectory"
unset history_subdirectory
PROMPT_COMMAND=${PROMPT_COMMAND+$PROMPT_COMMAND;}'history -a'

function hist {
    if hash ag >& "$null"; then
        ag "$@" "$history_directory"
    else
        grep -r "$@" "$history_directory"
    fi
}
