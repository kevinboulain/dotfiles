if hash luarocks &> "$null"; then
  eval $(luarocks path --bin)
fi
