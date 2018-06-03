# some Mac OS X specific stuff

# indicate to Finder whether it should show all files
finder_show_all() {
  local value=$(defaults read com.apple.finder AppleShowAllFiles 2> "$null")
  if [ "${value:-false}" = false ]; then
    value=true
  else
    value=false
  fi
  defaults write com.apple.finder AppleShowAllFiles "$value" && killall Finder
}
declare -rfx finder_show_all

# brew may have trouble in a virtualenv
brew() {
  [ -n "$VIRTUAL_ENV" ] && deactivate
  command brew "$@"
}
declare -rfx brew

export LC_ALL=en_US.UTF-8
export DICPATH=~/Library/Spelling

export HOMEBREW_NO_ANALYTICS=1
export HOMEBREW_CASK_OPTS="--appdir=~/Applications"
