# some options
shopt -s extglob # ls +!(boo*|?key*)
shopt -s checkwinsize # if not activated, will mess up the cli
complete -r # no completion

# handy variables, may be used in some sub scripts!
declare -r null=/dev/null

# find where this script is located, should work as long as it's only sourced
# by bash that give a complete path for ~/.bashrc
# note that readlink hasn't the -f option on OS X
config_directory=$(
    # cd into the symlink directory
    # cd into the directory of the file pointed by the (possibly relative) symlink
    # get actual path
    cd "$(dirname "${BASH_SOURCE[0]}")" \
    && cd "$(dirname "$(readlink "${BASH_SOURCE[0]}")")" \
    && pwd
)
# bash sub scripts directory
bash_config_directory=$config_directory/.bash

# source some 'extensions'
declare -a bash_config_files=(
    aliases
    path man editor pager
    prompt history
    lua
    "os/$(uname)"
    personal
)

for name in "${bash_config_files[@]}"; do
    script=$bash_config_directory/$name.bash
    [ -f "$script" ] && . "$script"
done

unset config_directory bash_config_files bash_config_directory script
