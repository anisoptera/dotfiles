# Vendored from https://github.com/lucasadelino/tide-item-jj (MIT, Lucas Adelino)
function _tide_item_vcs
    # Are we in a JJ repo?
    if test $PWD != $HOME
        and jj root &>/dev/null
        set in_jjdir true
    else
        set in_jjdir false
    end

    if test "$in_jjdir" = true
        _tide_item_jj
    else
        _tide_item_git
    end
end
