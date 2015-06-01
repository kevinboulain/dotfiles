#!/usr/bin/env bash

# commands executed at each command
function prepend_prompt_command {
    if [ $# -ne 1 ] || [ -z "$1" ]; then
        log "needs a string of commands to prepend to '\$PROMPT_COMMAND'"
        return 1
    fi

    local -r commands=$1
    prepend_to_var PROMPT_COMMAND ';' "$commands"
}

# commands executed at each command
function append_prompt_command {
    if [ $# -ne 1 ] || [ -z "$1" ]; then
        log "needs a string of commands to append to '\$PROMPT_COMMAND'"
        return 1
    fi

    local -r commands=$1
    append_to_var PROMPT_COMMAND ';' "$commands"
}

# some escape codes
cyan="\033[36m"
red="\033[31m"
green="\033[32m"
blue="\033[34m"
esc="\033[0m"

function prompt_config {
    if [ $? -eq 0 ]; then
        prompt_color=$cyan
    else
        prompt_color=$red
    fi

    # haskell
    prompt_haskell=
    sandboxed && prompt_haskell="λ"
    local -r sandbox=$(current_sandbox)
    [ -n "$sandbox" ] && prompt_haskell="λ'"

    # python
    prompt_python=
    local -r venv=$(current_venv)
    [ -n "$venv" ] && prompt_python="∞"

    prompt_languages="$prompt_python $prompt_haskell"
    if [[ "$prompt_languages" =~ ^[[:blank:]]+$ ]]; then
        prompt_languages=
    else
        prompt_languages="($(echo -n $prompt_languages))"
    fi
}

prepend_prompt_command prompt_config

# prompt, \[\]: allow readline to correctly calculate prompt size
PS1='$(echo -en "$prompt_languages")\h:\W \[$(echo -ne $prompt_color)\]\$\[$(echo -ne $esc)\] '
