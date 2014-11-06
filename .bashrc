#!/usr/bin/env bash

# some global variables that may influence
export PATH='/usr/local/bin:/usr/bin:/bin:/usr/sbin:/sbin'
export EDITOR='emacs'
export PAGER='less'

# handy variables
null='/dev/null'
# find where this script is located
# used in sub scripts!
config_directory=$(dirname $(readlink "$bashrc"))

# source some 'extensions'
for script in "$config_directory/bash/"{log,prompt,venvs}.bash; do
    . "$script"
done

# source os specific 'extension'
os_script="$config_directory/bash/os/`uname`.bash"
if [ -f "$os_script" ]; then
    . "$os_script"
fi

unset os_script

# some aliases
alias emacs='emacs -nw'
alias clean="find . -name '*.pyc' -delete -or -name '*~' -delete -or -name '*.o' -delete"
alias clean_list="find . -name '*.pyc' -or -name '*~' -or -name '*.o'"
alias l='ls'
alias lsvirtualenv='lsvirtualenv -b'
