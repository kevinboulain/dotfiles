#!/usr/bin/env bash

# handy variables
# used in sub scripts!
bashrc=~/.bashrc

# Mac OS X always run a login shell, so manually source .bashrc
[ -f "$bashrc" ] && . "$bashrc"
