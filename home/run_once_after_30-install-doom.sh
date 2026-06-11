#!/bin/sh
# Doom Emacs (replaces emacs/install.sh). ~/.doom.d itself is chezmoi-managed.
set -e
[ -d "$HOME/.emacs.d" ] && exit 0
git clone --depth 1 https://github.com/doomemacs/doomemacs "$HOME/.emacs.d"
"$HOME/.emacs.d/bin/doom" install
