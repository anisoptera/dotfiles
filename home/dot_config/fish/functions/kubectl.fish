function kubectl --wraps kubectl --description 'kubectl; reveals the tide kubectl segment on first use'
    if not contains kubectl $tide_right_prompt_items
        # Re-add the segment just left of the clock (or at the end), both in
        # the config list and in tide's runtime cache so it appears without
        # restarting the shell.
        for var in tide_right_prompt_items _tide_right_items
            set -l items $$var
            if set -l i (contains -i time $items); and test $i -gt 1
                set -U $var $items[1..(math $i - 1)] kubectl $items[$i..-1]
            else
                set -U $var $items kubectl
            end
        end
    end
    functions --erase kubectl
    command kubectl $argv
end
