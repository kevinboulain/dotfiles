if hash emacs >& "$null"; then
  EDITOR=emacs
elif hash mg >& "$null"; then
  EDITOR=mg
elif hash nano >& "$null"; then
  EDITOR=nano
fi

export EDITOR
