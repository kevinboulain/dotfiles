#!/usr/bin/env bash

# some notes:
# ncurses for emacs24-4
# LIBS=-lncurses ./configure --prefix=/home/ether/build/ --without-x LDFLAGS=-L/home/ether/build/lib/
# latest ghc for debian 7
# wget https://www.haskell.org/ghc/dist/7.8.4/ghc-7.8.4-x86_64-unknown-linux-deb7.tar.xz
# libgmp for ghc
# ln -s /usr/lib/x86_64-linux-gnu/libgmp.so.10 ~/build/lib/libgmp.so
# latest cabal, build with the bootstrap.sh script
# https://www.haskell.org/cabal/release/cabal-install-1.22.2.0/cabal-install-1.22.2.0.tar.gz

# user's build directory
# build=~/build
# if [ -d "$build" ]; then
#     export PATH="$build/bin:$PATH"
#     # hint for gcc
#     export LIBRARY_PATH="$build/lib"
#     # some specific binaries may not know where to search their libraries
#     # (ghc need a symlink for libgmp.so)
#     export LD_LIBRARY_PATH="$build/lib"
# fi

activate_default_venv "$config_directory"
activate_default_sandbox "$config_directory"

safe_prepend_to_path ~/build/bin
