function conda --description 'conda; loads the real conda hook on first use'
    # Lazy stand-in: sourcing the hook replaces this function with conda's
    # own (required for `conda activate`), so erase ourselves first and
    # re-dispatch. Note auto-activation of base, if enabled, also happens
    # here rather than at shell startup.
    functions --erase conda
    set -l conda_bin
    for candidate in /opt/homebrew/Caskroom/miniconda/base/bin/conda ~/miniconda3/bin/conda
        if test -x $candidate
            set conda_bin $candidate
            break
        end
    end
    if test -z "$conda_bin"
        echo "conda: no miniconda installation found" >&2
        return 127
    end
    $conda_bin shell.fish hook 2>/dev/null | source
    conda $argv
end
