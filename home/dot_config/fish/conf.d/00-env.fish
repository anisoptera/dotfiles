# Environment and PATH. Named 00- so it loads before the other conf.d files.

if test -x /opt/homebrew/bin/brew
    /opt/homebrew/bin/brew shellenv | source
end

set -gx EDITOR 'emacsclient -nw -a vim'

fish_add_path -g ~/bin ~/.local/bin ~/.cargo/bin

# GNU grep over BSD grep (brew 'grep')
fish_add_path -g /opt/homebrew/opt/grep/libexec/gnubin
