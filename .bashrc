#!/usr/bin/env bash

# some global variables that may influence
export PATH='/usr/local/bin:/usr/bin:/bin:/usr/sbin:/sbin'
export EDITOR='emacs'
export PAGER='less'

# handy variables
null='/dev/null'
# may be defined in .bash_profile
if [ ! -n "$bashrc" ]; then
    bashrc=~/.bashrc
fi
# find where this script is located, used in some sub scripts!
config_directory=$(dirname $(readlink "$bashrc"))
# bash sub scripts directory
bash_directory="$config_directory/bash"

# source some 'extensions'
for script in "$bash_directory/"{log,prompt,venvs}.bash; do
    . "$script"
done

# source os specific 'extension'
os_script="$bash_directory/os/`uname`.bash"
if [ -f "$os_script" ]; then
    . "$os_script"
fi

unset os_script bash_directory

# some aliases
hash ag >& "$null"
if [ $? -eq 0 ]; then
    alias grep='ag'
fi
alias emacs='emacs -nw'
alias clean="find . -name '*.pyc' -delete -or -name '*~' -delete -or -name '*.o' -delete"
alias clean_list="find . -name '*.pyc' -or -name '*~' -or -name '*.o'"
alias l='ls'
alias lsvirtualenv='lsvirtualenv -b'
