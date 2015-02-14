#!/usr/bin/env bash

# some global variables that may influence
export PATH='/usr/local/bin:/usr/bin:/bin:/usr/sbin:/sbin'
export MANPATH=$(man --path)
export EDITOR='emacs'
export PAGER='less'

# handy variables, may be used in some sub scripts!
null='/dev/null'
# may already be defined via .bash_profile
if [ ! -n "$bashrc" ]; then bashrc=~/.bashrc; fi
# find where this script is located
config_directory=$(dirname $(readlink "$bashrc"))
# bash sub scripts directory
bash_directory="$config_directory/bash"

# source some 'extensions'
for script in "$bash_directory/"{log,alias,prompt,python,os/`uname`}.bash; do
    if [ -f "$script" ]; then
        . "$script"
    fi
done

unset bash_directory

# some aliases
alias l='ls'
