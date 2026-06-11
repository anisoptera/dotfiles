function gf --description 'Check out a remote branch locally'
    git checkout -b $argv[1] origin/$argv[1]
end
