if hash most >& "$null"; then
    PAGER=most
elif hash less >& "$null"; then
    PAGER='less -R'
fi

export PAGER
