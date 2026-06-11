# Companion to tide_jj_enable / the vendored _tide_item_vcs item.
# Tide's _tide_cache_variables only sets the branch/icon color when the item
# list literally contains 'git'; set it here for the swapped-in 'vcs' item.
# (Runs before first prompt; the cache function won't overwrite it since its
# own 'contains git' check fails.)
if status is-interactive
    and contains vcs $tide_left_prompt_items $tide_right_prompt_items
    set_color $tide_git_color_branch | read -gx _tide_location_color
end

# Icon for the jj segment; no official nerd-font glyph exists yet, so default
# to plain text. Override with: set -U tide_jj_icon <glyph>
set -q tide_jj_icon; or set -g tide_jj_icon jj
