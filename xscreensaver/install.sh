#!/bin/zsh

mkdir -p ~/.config/systemd/user
ln -s ~/.dotfiles/xscreensaver/xscreensaver.service ~/.config/systemd/user
systemctl --user enable xscreensaver
