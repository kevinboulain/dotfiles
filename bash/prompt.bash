#!/usr/bin/env bash

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
PS1='\h:\W \[$(echo -ne $prompt_color)\]\$\[$(echo -ne $esc)\] '
