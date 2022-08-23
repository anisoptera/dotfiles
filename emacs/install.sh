#!/bin/bash
[ -d ~/.emacs.d ] || (git clone https://github.com/syl20bnr/spacemacs ~/.emacs.d ; ~/.emacs.d/bin/doom install)
