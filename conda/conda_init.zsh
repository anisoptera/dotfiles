#!/usr/bin/env zsh

# >>> conda initialize >>>
# !! Contents within this block are managed by 'conda init' !!
if [ -d "/opt/homebrew/Caskroom/miniconda" ]; then
    __conda_setup="$('/opt/homebrew/Caskroom/miniconda/base/bin/conda' 'shell.zsh' 'hook' 2> /dev/null)"
    if [ $? -eq 0 ]; then
        eval "$__conda_setup"
    else
        if [ -f "/opt/homebrew/Caskroom/miniconda/base/etc/profile.d/conda.sh" ]; then
            . "/opt/homebrew/Caskroom/miniconda/base/etc/profile.d/conda.sh"
        else
            export PATH="/opt/homebrew/Caskroom/miniconda/base/bin:$PATH"
        fi
    fi
    unset __conda_setup
fi
# <<< conda initialize <<<


# >>> conda initialize >>>
# !! Contents within this block are managed by 'conda init' !!
if [ -d "$HOME/miniconda3" ]; then
   __conda_setup="$('$HOME/miniconda3/bin/conda' 'shell.zsh' 'hook' 2> /dev/null)"
   if [ $? -eq 0 ]; then
       eval "$__conda_setup"
   else
       if [ -f "$HOME/miniconda3/etc/profile.d/conda.sh" ]; then
           . "$HOME/miniconda3/etc/profile.d/conda.sh"
       else
           export PATH="$HOME/miniconda3/bin:$PATH"
       fi
   fi
   unset __conda_setup
fi
# <<< conda initialize <<<
#
