#!/usr/bin/env bash

# ignore duplicates in history
HISTCONTROL=ignoredups:erasedups

# append to history, do not overwrite it
shopt -s histappend

# http://unix.stackexchange.com/a/18443
append_prompt_command 'history -n; history -w; history -c; history -r'
