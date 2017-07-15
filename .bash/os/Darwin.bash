# some Mac OS X specific stuff

# indicate to Finder whether it should show all files or not
function finder_show_all {
  local -r old_value=$(defaults read com.apple.finder AppleShowAllFiles 2> "$null")
  local new_value
  if [ "${old_value-false}" = false ]; then
    new_value=true
  else
    new_value=false
  fi
  defaults write com.apple.finder AppleShowAllFiles "$new_value" &&
    killall Finder
}

# brew may have trouble in a virtualenv
function brew {
  [ -n "$VIRTUAL_ENV" ] && deactivate
  /usr/local/bin/brew "$@"
}

export LC_ALL=en_US.UTF-8
export DICPATH=~/Library/Spelling
