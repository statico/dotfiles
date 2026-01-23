# 💾 Ian's Dotfiles and Neovim config

[![build status](https://img.shields.io/github/actions/workflow/status/statico/dotfiles/build.yml?branch=main&style=flat-square)](https://ghcr.io/statico/dotfiles)

These are my dotfiles. There are many like them, but these are mine. This started around 2001 or so when I used many different operating systems and environments. Now I mostly use macOS and Linux but still keep the environment in sync.

<img width="2898" height="1668" alt="CleanShot 2026-01-22 at 21 42 05@2x" src="https://github.com/user-attachments/assets/c238bfa8-3223-47c8-a016-92a8b58f4105" />

Favorite font at the moment: [Iosevka Term](https://typeof.net/Iosevka/)

### Demo

```
$ docker run --rm -it ghcr.io/statico/dotfiles
demouser@987552d4c629:/
〉
```

A random prompt color is picked at first install. Edit `.zshlocal` for details.

### Installation

1. `curl -sL statico.link/zsh | zsh` (redirects to the `install.zsh` file in this repo)
1. `chsh` or otherwise set your shell to use [Zsh](http://www.zsh.org/)
1. Restart your shell

Optionally create a `.postinstall` with some machine-specific commands, like `git config --global user.email "my-work-email@example.com"`". You can also create a `.vimlocal` and `.gvimlocal` for machine-specific Vim customizations (though I primarily use Neovim now, the Vim config is kept for historical reasons).

### Customizing

- Fork this repo
- Update `install.zsh` to point at your own
- Go nuts

### Highlights (or, Why I Use This)

My first boss told me that I should have a digital _toolkit_ -- a set of tools that I keep with me that I can use anywhere. In college I used many different kinds of Unix variants, and so it made sense to build a kit that could bootstrap my environment anywhere. Today, I use macOS, Windows, and various distributions of Linux, and this kit has proven invaluable in getting set up quickly on new hardware and VMs.

While this project includes a huge collection of configuration snippets I've collected over the years, a few parts stand out and are things I use daily:

- My [`zshrc`](https://github.com/statico/dotfiles/blob/main/.zshrc), which has a minimal, customized prompt that I like the best, as well as many aliases and shortcuts to standardize environments (like making sure Unicode displays properly and `ls` shows colors) and reduce keystrokes (like with my ~60 or so Git aliases). There are also a few functions that make host-specific customizations easy using a `.zshlocal` script which never gets checked in. There are tons of tricks in there so it's worth a skim.

- My Neovim configuration (in `.config/nvim/init.lua`), which I use as my primary editor. I also keep my old [`vimrc`](https://github.com/statico/dotfiles/blob/main/.vim/vimrc) for historical reasons, which many people became interested in after reading my [articles about Vim](https://blog.langworth.com).

### Why Zsh instead of Bash?

Only a few reasons, honestly:

- Easier completion - I can type `/u/l/b/x`<kbd>Tab</kbd> and that completes to `/usr/local/bin/x`

- I'm able to hack <kbd>Ctrl-W</kbd> to delete to the previous word _or_ slash, so `/usr/local/bin`<kbd>Ctrl-W</kbd> becomes `/usr/local/`

- I've got a thing that shows me five red dots when a completion is in progress, such as when completing files from remote SSH servers.

- Globbing - The `**` recursive operator and qualifiers like `(.)` and `(/)` for globbing are essential, like `rm **/.DS_Store`

- Legacy - I started using Zsh in 2002 or so when it was edgy.

### Common Tricks

Here's what I use the most often on the command line:

- `j foo` to `cd` to the most commonly used directory that fuzzy-matches foo (via [autojump](https://github.com/wting/autojump))
- `l` and `ll` for long directory listings, `ltr` for showing the most recent files
- Other single-character aliases: `g` for `git`, `d` for `docker`, `dc` for `docker-compose`, `k` for `tree`, `y` for `yarn`
- <kbd>Meta-L</kbd> which appends `2>&1|less` to the command and hits <kbd>Enter</kbd>, running the command and viewing its output in a pager
- `ZU` to update Neovim plugins or just `ZR` to restart Zsh after a .zshrc change
- Searching with `rg` ([ripgrep](https://github.com/BurntSushi/ripgrep)), then <kbd>Ctrl-A</kbd><kbd>v</kbd><kbd>Enter</kbd> (changing `rg` to `vrg`) to edit all of the files that matched in Neovim
- `cd`ing to a directory and then using <kbd>Meta-P</kbd> to pop to the previous directory (since `auto_pushd` is enabled and silent)
- `psl` to search for processes (since I never remember the `pgrep` syntax and it's never been consistent across platforms)
- `open` and `trash` commands that work across macOS and Linux
- If I'm typing a command but realize that I need to do something else first, <kbd>Meta-Q</kbd> queues the current command and clears the command line, then pastes it back in after I enter and run another command first.
- Git commands: `st` for status, `gd` for a git diff, `gl` for a quick log, `sci <message>` to commit everything with a message, or `gap` to cherry pick and then `gc <message>` to commit.
- <kbd>Ctrl-G Ctrl-G</kbd> quick switches to a git branch thanks to [fzf-git.sh](https://github.com/junegunn/fzf-git.sh)
- Fuzzy history search using <kbd>Ctrl-R</kbd> and [FZF](https://github.com/junegunn/fzf)
- Each host gets a different `colorprompt` command in its `~/.zshlocal`. I use `ansimodes` or `256-colors.sh` to pick a color. (Both are already in the `~/bin/` directory, which is added to the `$PATH`.)

### See also

- [Oh My Zsh](https://github.com/robbyrussell/oh-my-zsh) which is a very popular way of customizing Zsh (but not all dotfiles or Neovim)
- http://github.com/jbalogh/dotfiles which this was modeled after
