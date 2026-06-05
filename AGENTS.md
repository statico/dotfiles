# Dotfiles

Personal dotfiles — **public and shared across many hosts.** Files here get **symlinked into `~/`** — so edit them *in this repo*, not in your home directory. Never check in host-specific or secret values; put those in local overrides (`~/.zshlocal`, `~/.postinstall`, `~/.config/nvim/nvimlocal.lua`), which are not in this repo. `install.zsh` does the linking: top-level `.*` files → `~/`, `.config/*/*` → `~/.config/`, `bin/*` → `~/bin/`. It also installs Homebrew pkgs (`homebrew.txt`), zsh completions, tmux/TPM, and macOS defaults (`bin/macos-setup`). Re-run with `./install.zsh` (or it self-clones from GitHub on a fresh machine).

## Key configs

- **Neovim**: `.config/nvim/init.lua` (everything's in one file)
- **Ghostty**: `.config/ghostty/config`
- **Shell**: `.zshrc`, plugins in `.zsh_plugins/`
- **tmux**: `.tmux.conf`
- **git**: `.gitconfig.base` (copied, not symlinked, to `~/.gitconfig`, so `~/.postinstall` can override it)
- Machine-local overrides (gitignored, not in repo): `~/.zshlocal`, `~/.config/nvim/nvimlocal.lua`, `~/.postinstall`
