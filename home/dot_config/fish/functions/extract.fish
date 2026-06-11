function extract --description 'Extract archives / mount disk images'
    if not test -f $argv[1]
        echo "'$argv[1]' is not a valid file"
        return 1
    end
    switch $argv[1]
        case '*.tar.bz2' '*.tbz2'
            tar -jxvf $argv[1]
        case '*.tar.gz' '*.tgz'
            tar -zxvf $argv[1]
        case '*.bz2'
            bunzip2 $argv[1]
        case '*.dmg'
            hdiutil mount $argv[1]
        case '*.gz'
            gunzip $argv[1]
        case '*.tar'
            tar -xvf $argv[1]
        case '*.zip' '*.ZIP'
            unzip $argv[1]
        case '*.pax'
            cat $argv[1] | pax -r
        case '*.pax.Z'
            uncompress $argv[1] --stdout | pax -r
        case '*.rar'
            unrar x $argv[1]
        case '*.Z'
            uncompress $argv[1]
        case '*'
            echo "'$argv[1]' cannot be extracted/mounted via extract"
            return 1
    end
end
