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
)/bash

# the whole configuration is documented in the readme.org file
# can't source process substitution in Bash 3
eval "$(sed '/^#+begin_src shell$/,/^#+end_src$/!d;//d' "$config_directory"/readme.org)"

if [ -f "$config_directory"/local.bash ]; then
  . "$config_directory"/local.bash
fi

unset config_directory
