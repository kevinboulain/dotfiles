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

# the whole configuration is documented in the readme.org file
# can't source process substitution in Bash 3
eval "$(sed '/^#+begin_src shell$/,/^#+end_src$/!d;//d' "$bash_config_directory"/readme.org)"

declare -a bash_files=(
  # optional local configuration
  local
)

for name in "${bash_files[@]}"; do
  script=$bash_config_directory/$name.bash
  [ -f "$script" ] && . "$script"
done

unset config_directory bash_files bash_config_directory script name
