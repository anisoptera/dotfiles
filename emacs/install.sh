#!/bin/bash
[ -d ~/.emacs.d ] || (git clone --depth 1 https://github.com/doomemacs/doomemacs ~/.emacs.d ; ~/.emacs.d/bin/doom install)
# [ -d ~/.spacemacs.d ] || 
