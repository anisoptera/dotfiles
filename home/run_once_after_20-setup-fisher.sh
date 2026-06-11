#!/bin/sh
# Bootstraps fisher, which then installs everything in ~/.config/fish/fish_plugins
# (tide prompt, puffer-fish !!/!$ expansion).
set -e
FISH="$(command -v fish || echo /opt/homebrew/bin/fish)"
"$FISH" -c '
if not functions -q fisher
    curl -sL https://raw.githubusercontent.com/jorgebucaran/fisher/main/functions/fisher.fish | source
    fisher update
end'
echo "==> fisher + plugins installed. Run: tide configure (inside fish) to pick a prompt style."
