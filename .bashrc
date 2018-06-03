set -u

# some options
shopt -s extglob # ls +!(boo*|?key*)
shopt -s checkwinsize # if not activated, will mess up the cli
shopt -s cdspell # cd correct typos
shopt -s direxpand # allow bash to edit directories during tab completion (useful with dirspell)
shopt -s dirspell # fix typos on directories during tab completion
shopt -s no_empty_cmd_completion # disable tab completion on empty line
complete -r # no completion
set -o emacs # emacs mode

# handy variables, may be used in some sub scripts!
declare -r null=/dev/null

# find where this script is located, should work as long as it's only sourced
# by bash that give a complete path for ~/.bashrc
# note that readlink hasn't the -f option on OS X
config_directory=$(
  # cd into the symlink directory
  # cd into the directory of the file pointed by the (possibly relative) symlink
  # get actual path
  cd "$(dirname "${BASH_SOURCE[0]}")" > /dev/null &&
  cd "$(dirname "$(readlink "${BASH_SOURCE[0]}")")" > /dev/null &&
  pwd
)
# bash sub scripts directory
bash_config_directory=$config_directory/bash

declare -a bash_files=(
  aliases
  path man editor pager
  prompt history
  lua
  "os/$(uname)"
  personal
)

for name in "${bash_files[@]}"; do
  script=$bash_config_directory/$name.bash
  [ -f "$script" ] && . "$script"
done

unset config_directory bash_files bash_config_directory script name

set +u
