#!/usr/bin/env bash

# some OpenBSD specific stuff

export PKG_PATH="http://ftp.fr.openbsd.org/pub/OpenBSD/`uname -r`/packages/`arch -s`"

activate_default_venv ~/stuff
