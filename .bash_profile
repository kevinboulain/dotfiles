#!/usr/bin/env bash

bashrc=~/.bashrc

# Mac OS X always run a login shell, so manually source .bashrc
if [ -f "$bashrc" ]; then
    . "$bashrc"
fi
