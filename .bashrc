# some global variables that may influence
export EDITOR=emacs
export PAGER=less

# some aliases
alias l=ls

# some options
shopt -s extglob # ls +!(boo*|?key*)
shopt -s checkwinsize # if not activated, will mess up the cli
complete -r

# handy variables, may be used in some sub scripts!
null=/dev/null

# find where this script is located, should work as long as it's only sourced
# by bash that give a complete path for ~/.bashrc
config_directory=$(dirname $(readlink "${BASH_SOURCE[0]}"))
# bash sub scripts directory
bash_config_directory=$config_directory/bash

# source some 'extensions'
declare -a bash_config_files=(
    log
    helpers
    path man
    prompt history
    lua
    haskell python
    "os/$(uname)"
)

for name in "${bash_config_files[@]}"; do
    script=$bash_config_directory/$name.bash
    [ -f "$script" ] && . "$script"
    unset script
done

unset bash_config_files bash_config_directory config_directory
