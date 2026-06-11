# git
alias gl 'git pull --prune'
alias glog "git log --graph --pretty=format:'%Cred%h%Creset %an: %s - %Creset %C(yellow)%d%Creset %Cgreen(%cr)%Creset' --abbrev-commit --date=relative"
alias gp 'git push origin HEAD'
alias gpp 'git push origin (git symbolic-ref --short -q HEAD)'
alias gd 'git diff --color | sed "s/^\([^-+ ]*\)[-+ ]/\\1/" | less -r'
alias gc 'git commit'
alias gca 'git commit -a'
alias gco 'git checkout'
alias gcb 'git copy-branch-name'
alias gb 'git branch'
alias gs 'git status -sb'
alias gac 'git add -A; and git commit -m'
alias ge 'git-edit-new'

# docker
alias d docker
alias d-c 'docker compose'

# kubernetes
alias k kubectl

# emacs
alias e 'emacsclient -n'
alias en 'emacsclient -n -c'
alias et 'emacsclient -nw -a vim'

# xcode
alias ios 'open /Applications/Xcode.app/Contents/Developer/Applications/Simulator.app'
alias watchos 'open "/Applications/Xcode.app/Contents/Developer/Applications/Simulator (Watch).app"'

# system
alias pubkey 'cat ~/.ssh/id_rsa.pub | pbcopy; and echo "=> Public key copied to pasteboard."'
alias p pueue
alias cls clear
alias reload! 'exec fish'
alias tree 'tree -a -I .git'

# coreutils ls (brew 'coreutils')
if type -q gls
    alias ls 'gls -F --color'
    alias l 'gls -lAh --color'
    alias ll 'gls -l --color'
    alias la 'gls -A --color'
end
