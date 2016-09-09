if hash less >& "$null"; then
    PAGER='less -R'
elif hash most >& "$null"; then
    PAGER=most
fi

export PAGER
