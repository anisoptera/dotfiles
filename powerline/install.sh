pip install --user powerline-status
POWERLINE_PREFIX=`pip show powerline-status | perl -ne 'print "$1" if /Location: (.*)/'`
ln -sf $POWERLINE_PREFIX/powerline/bindings/tmux/powerline.conf $HOME/.config/tmux-powerline.conf
