if status is-interactive
    set -gx GPG_TTY (tty)

    type -q mise; and mise activate fish | source
    type -q zoxide; and zoxide init fish | source
    type -q direnv; and direnv hook fish | source

    # GRC colorizes nifty unix tools all over the place
    test -r "$HOMEBREW_PREFIX/etc/grc.fish"; and source "$HOMEBREW_PREFIX/etc/grc.fish"

    test -e ~/.iterm2_shell_integration.fish; and source ~/.iterm2_shell_integration.fish
end
