# This file takes precedence over ~/.profile.
if [ -f ~/.profile ]; then
  . ~/.profile
fi

# macOS always runs a login shell, manually source ~/.bashrc.
if [ -f ~/.bashrc ]; then
  . ~/.bashrc
fi
