#!/usr/bin/env bash

# may already be defined via .bash_profile
[ ! -n "$bashrc" ] && bashrc=~/.bashrc
# find where this script is located, must be a symlink
[ -L "$bashrc" ] || return 1
config_directory=$(dirname $(readlink "$bashrc"))

# some global variables that may influence
export EDITOR=emacs
export PAGER=less

# handy variables, may be used in some sub scripts!
null=/dev/null
# bash sub scripts directory
bash_directory=$config_directory/bash

# source some 'extensions'
declare -a bash_files=(\
    helpers \
    log \
    path man \
    prompt history \
    haskell python \
    "os/$(uname)" \
)
for name in "${bash_files[@]}"; do
    script=$bash_directory/$name.bash
    [ -f "$script" ] && . "$script"
    unset script
done
unset bash_files

unset bash_directory

# some aliases
alias l=ls

# some options
shopt -s extglob # ls +!(boo*|?key*)
