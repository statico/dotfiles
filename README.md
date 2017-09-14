# 💾 Ian's Dotfiles and Vim config

These are my dotfiles. There are many like them, but these are mine.

This started around 2001 or so when I used lots of different machines and environments. Now I mostly use macOS and Linux but still keep the environment in sync.

### Installation

1. `curl -sL https://raw.githubusercontent.com/statico/dotfiles/master/install.sh | bash`
1. `chsh` or otherwise set your shell to use [Zsh](http://www.zsh.org/)
1. Restart your shell

Optionally create a `.postinstall` with some machine-specific commands, like `git config --global user.email "my-work-email@example.com"`". You can also create a `.vimlocal` and `.gvimlocal` for machine-specific Vim customizations.

### Customizing

- Fork this repo
- Update `install.sh` to point at your own
- Go nuts

### See also

- [Oh My Zsh](https://github.com/robbyrussell/oh-my-zsh) which is a very popular way of customizing Zsh (but not all dotfiles or Vim)
- http://github.com/jbalogh/dotfiles which this was modeled after
