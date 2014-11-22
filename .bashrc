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
for script in "$bash_directory/"{log,alias,prompt,venvs,os/`uname`}.bash; do
    if [ -f "$script" ]; then
        . "$script"
    fi
done

unset bash_directory

# some aliases
alias_existing_command 'grep' 'ag'
alias_existing_command 'clamscan' 'clamscan -i' # show only errors
alias_existing_command 'lsvirtualenv' 'lsvirtualenv -b' # brief mode
alias_existing_command 'emacs' 'emacs -nw' # no window
alias clean="find . -name '*~' -delete"
alias clean_list="find . -name '*~'"
alias l='ls'
