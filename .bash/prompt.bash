# some escape codes
declare -r cyan="\033[36m"
declare -r red="\033[31m"
declare -r green="\033[32m"
declare -r blue="\033[34m"
declare -r esc="\033[0m"

function prompt_config {
    if [ $? -eq 0 ]; then
        prompt_color=$cyan
    else
        prompt_color=$red
    fi
}

# on OS X, $PROMPT_COMMAND may use the function update_terminal_cwd
PROMPT_COMMAND=${PROMPT_COMMAND+$PROMPT_COMMAND;}prompt_config

# \[\] allow readline to correctly calculate string size
PS1='\h:\W \[$(echo -ne $prompt_color)\]\$\[$(echo -ne $esc)\] '
