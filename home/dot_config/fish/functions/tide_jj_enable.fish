function tide_jj_enable --description 'Swap tide git item for the jj-aware vcs item'
    # tide configure rewrites these universal lists with a plain git item,
    # so re-run this after reconfiguring tide.
    set -U tide_left_prompt_items (string replace -r -- '^git$' vcs $tide_left_prompt_items)
    set -U tide_right_prompt_items (string replace -r -- '^git$' vcs $tide_right_prompt_items)

    # _tide_print_item jj needs jj segment colors; inherit git's.
    set -q tide_jj_bg_color; or set -U tide_jj_bg_color $tide_git_bg_color
    set -q tide_jj_color; or set -U tide_jj_color $tide_git_color

    echo "tide items now: $tide_left_prompt_items | $tide_right_prompt_items"
end
