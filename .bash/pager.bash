if hash less >& "$null"; then
  PAGER='less -r'
elif hash most >& "$null"; then
  PAGER=most
fi

export PAGER
