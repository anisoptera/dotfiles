# dotfiles

Personal dotfiles, managed with [chezmoi](https://www.chezmoi.io/).

## Install

```sh
sh -c "$(curl -fsLS get.chezmoi.io)" -- init --apply <repo-url>
```

On first run chezmoi prompts for a git author name and email (stored per
machine in `~/.config/chezmoi/chezmoi.toml`, never in this repo), then:

- bootstraps Homebrew if missing and installs everything in `~/.Brewfile`
  (re-running whenever the Brewfile changes)
- installs [fisher](https://github.com/jorgebucaran/fisher) and the fish
  plugins in `fish_plugins`
- clones and installs [Doom Emacs](https://github.com/doomemacs/doomemacs)

## What's inside

The chezmoi source lives in `home/` (see `.chezmoiroot`). Highlights:

- **fish + [Tide](https://github.com/IlanCosman/tide)** prompt with
  [jujutsu](https://github.com/jj-vcs/jj) awareness, via a vendored copy of
  [tide-item-jj](https://github.com/lucasadelino/tide-item-jj). Run
  `tide_jj_enable` after any `tide configure` to restore the vcs item.
- **Lazy loading** for slow shell hooks: the kubectl prompt segment stays
  hidden until kubectl is first used, and conda's hook loads on first call.
- **tmux** themed with [catppuccin](https://github.com/catppuccin/tmux)
  (pinned in `.chezmoiexternal.toml`), plus `bin/tmux-agent-watch`: windows
  running agent CLIs (claude, codex, ...) get a yellow accent when they
  produce output you haven't seen and a green accent (plus one bell) when
  they go idle afterwards — cleared by visiting the window.
- **Doom Emacs** config in `doom.d/`, symlinked to `~/.doom.d`.
- **mise** for runtime version management, **zoxide**, **direnv**.
- A zsh setup that predates the fish migration, kept working as a fallback.

The other top-level directories are the repo's previous
topic-per-directory layout (descended from holman/dotfiles); they are
superseded by `home/` and will be removed once the migration fully settles.

## License

[AGPL-3.0](LICENSE). This repo began life as a fork of
[holman/dotfiles](https://github.com/holman/dotfiles) (MIT) and still
contains code descended from it; vendored third-party files such as
tide-item-jj (MIT) retain their original license notices in-file.
