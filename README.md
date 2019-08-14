# 💾 Ian's Dotfiles and Vim config

These are my dotfiles. There are many like them, but these are mine.

This started around 2001 or so when I used lots of different machines and environments. Now I mostly use macOS and Linux but still keep the environment in sync.

### Installation

1. `curl -sL https://zsh.langworth.com | zsh`
1. `chsh` or otherwise set your shell to use [Zsh](http://www.zsh.org/)
1. Restart your shell

Optionally create a `.postinstall` with some machine-specific commands, like `git config --global user.email "my-work-email@example.com"`". You can also create a `.vimlocal` and `.gvimlocal` for machine-specific Vim customizations.

### Customizing

- Fork this repo
- Update `install.zsh` to point at your own
- Go nuts

### Highlights (or, Why I Use This)

My first boss told me that I should have a digital _toolkit_ -- a set of tools that I keep with me that I can use anywhere. In college I used many different kinds of Unix variants, and so it made sense to build a kit that could bootstrap my environment anywhere. Today, I use macOS, Windows, and various distributions of Linux, and this kit has proven invaluable in getting set up quickly on new hardware and VMs.

While this project includes a huge collection of configuration snippets I've collected over the years, a few parts stand out and are things I use daily:

- My [`zshrc`](https://github.com/statico/dotfiles/blob/master/.zshrc), which has a minimal, customized prompt that I like the best, as well as many aliases and shortcuts to standardize environments (like making sure Unicode displays properly and `ls` shows colors) and reduce keystrokes (like with my ~60 or so Git aliases). There are also a few functions that make host-specific customizations easy using a `.zshlocal` script which never gets checked in. There are tons of tricks in there so it's worth a skim.

- My [`vimrc`](https://github.com/statico/dotfiles/blob/master/.vim/vimrc), which many people became interested in after reading my [articles about Vim](https://statico.github.io). I also have [an `update.sh` script](https://github.com/statico/dotfiles/blob/master/.vim/update.sh) which installs all of the Vim plugins and themes I like to use, and it gets run as part of the Zsh update process (aliased to `ZU`). Is my Vim update thing better than Vundle or another plugin manager? Maybe. It's very simple and fast and works everywhere, so I stick with it.

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
- `ZU` to update Vim plugins or just `ZR` to restart Zsh after a .zshrc change
- Searching with `rg` ([ripgrep](https://github.com/BurntSushi/ripgrep)), then <kbd>Ctrl-A</kbd><kbd>v</kbd><kbd>Enter</kbd> (changing `rg` to `vrg`) to edit all of the files that matched in Vim
- `cd`ing to a directory and then using <kbd>Meta-P</kbd> to pop to the previous directory (since `auto_pushd` is enabled and silent)
- `psl` to search for processes (since I never remember the `pgrep` syntax and it's never been consistent across platforms)
- `open` and `trash` commands that work across macOS and Linux
- If I'm typing a command but realize that I need to do something else first, <kbd>Meta-Q</kbd> queues the current command and clears the command line, then pastes it back in after I enter and run another command first.
- Git commands: `st` for status, `gd` for a git diff, `gl` for a quick log, `sci <message>` to commit everything with a message, or `gap` to cherry pick and then `gc <message>` to commit.
- Fuzzy history search using <kbd>Ctrl-R</kbd> and [FZF](https://github.com/junegunn/fzf)
- Each host gets a different `colorprompt` command in its `~/.zshlocal`. I use `ansimodes` or `256-colors.sh` to pick a color. (Both are already in the `~/bin/` directory, which is added to the `$PATH`.)

### Testing

Testing is easy with Docker:

```
$ docker build . --tag dotfiles
$ docker run --rm -it dotfiles
root@987552d4c629:/
〉
```

### See also

- [Oh My Zsh](https://github.com/robbyrussell/oh-my-zsh) which is a very popular way of customizing Zsh (but not all dotfiles or Vim)
- http://github.com/jbalogh/dotfiles which this was modeled after
