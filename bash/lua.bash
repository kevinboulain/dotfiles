#!/usr/bin/env bash

# require luarocks
if hash luarocks &> "$null"; then
    eval $(luarocks path --bin)
fi
