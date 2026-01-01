#!/usr/bin/env zsh
{ # This ensures the entire script is downloaded.

set -eo pipefail

basedir="$HOME/.dotfiles"
bindir="$HOME/bin"
repourl="https://github.com/statico/dotfiles.git"

function symlink() {
  src="$1"
  dest="$2"

  # Create parent directory if it doesn't exist
  dest_dir=$(dirname "$dest")
  if [ ! -d "$dest_dir" ]; then
    echo "→ Creating directory $dest_dir"
    mkdir -p "$dest_dir"
  fi

  if [ -e "$dest" ]; then
    if [ -L "$dest" ]; then
      if [ ! -e "$dest" ]; then
        echo "→ Removing broken symlink at $dest"
        rm "$dest"
      else
        # Already symlinked -- I'll assume correctly.
        return 0
      fi
    else
      # Rename files with a ".old" extension.
      echo "⊟ $dest already exists, renaming to $dest.old"
      backup="$dest.old"
      if [ -e "$backup" ]; then
        echo "✗ Error: "$backup" already exists. Please delete or rename it."
        exit 1
      fi
      mv "$dest" "$backup"
    fi
  fi
  ln -sf "$src" "$dest"
}

if ! which git >/dev/null ; then
  echo "✗ Error: git is not installed"
  exit 1
fi

if [ -d "$basedir/.git" ]; then
  echo "⟲ Updating dotfiles using existing git..."
  cd "$basedir"
  git pull --quiet --rebase origin main || exit 1
else
  echo "↓ Checking out dotfiles using git..."
  rm -rf "$basedir"
  git clone --quiet --depth=1 "$repourl" "$basedir"
fi

cd "$basedir"

echo "→ Cleaning up dead symlinks..."
find "$HOME" -maxdepth 1 -type l ! -exec test -e {} \; -delete
[ -d "$bindir" ] && find "$bindir" -maxdepth 1 -type l ! -exec test -e {} \; -delete

echo "⚡Updating common Zsh completions..."
rm -rf .zsh-completions ~/.zcompdump
git clone --quiet --depth=1 https://github.com/zsh-users/zsh-completions .zsh-completions

echo "→ Creating symlinks..."
for item in .* ; do
  case "$item" in
    .|..|.git|.config)
      continue
      ;;
    *)
      symlink "$basedir/$item" "$HOME/$item"
      ;;
  esac
done

symlink "$basedir/.vim/vimrc" "$HOME/.vimrc"
symlink "$basedir/.vim/gvimrc" "$HOME/.gvimrc"

for item in .config/*/* ; do
  if [ -f "$item" ]; then
    rel_path=${item#.config/}
    symlink "$basedir/$item" "$HOME/.config/$rel_path"
  fi
done

if [ "$(uname -s)" = "Darwin" ]; then
  vscodepath="$HOME/Library/Application Support/Code/User"
  vscodeplatform=mac
else
  vscodepath="$HOME/.config/Code/User"
  vscodeplatform=linux
fi
mkdir -p "$vscodepath"
symlink "$basedir/.vscode-$vscodeplatform.settings.json" "$vscodepath/settings.json"
symlink "$basedir/.vscode-$vscodeplatform.keybindings.json" "$vscodepath/keybindings.json"
symlink "$basedir/.vscode-snippets" "$vscodepath/snippets"

echo "⚙ Adding executables to ~/bin/..."
mkdir -p "$bindir"
for item in bin/* ; do
  symlink "$basedir/$item" "$bindir/$(basename $item)"
done

if [ -n "$VSCODE_REMOTE_CONTAINERS_SESSION" ]; then
  # We must be setting up a VS Code remote dev container, so I probably won't use Vim.
  echo "◉ VS Code remote environment detected. Skipping Vim setup."
else
  echo "◉ Setting up vim plugins..."
  .vim/update.sh
fi

if [ -e "$HOME/Library" ]; then
  echo "⌨ Adding extra keybindings to macOS..."
  mkdir -p "$HOME/Library/KeyBindings"
  symlink "$basedir/DefaultKeyBinding.dict" "$HOME/Library/KeyBindings/DefaultKeyBinding.dict"
fi

echo "◉ Setting up git..."
if [ -n "$VSCODE_REMOTE_CONTAINERS_SESSION" ]; then
  echo "◉ VSCode remote container detected"
  # VS Code won't add a .gitconfig if one already exists, so we need to put
  # ours in a magical secondary location I found by reading the Git docs.
  altdir="$HOME/.althome"
  mkdir -p "$altdir/git"
  echo "export XDG_CONFIG_HOME=\"$altdir\"" >>"$HOME/.zshlocal"
  cp "$basedir/.gitconfig.base" "$altdir/git/config"
else
  cp "$basedir/.gitconfig.base" "$HOME/.gitconfig"
fi

if which git-lfs >/dev/null 2>&1 ; then
  echo "◉ Installing git-lfs"
  git lfs install
fi

if which ksdiff >/dev/null 2>&1 ; then
  echo "◉ Found Kaleidoscope.app diff tool (ksdiff). Configuring git to use it."
  git config --global difftool.Kaleidoscope.cmd 'ksdiff --partial-changeset --relative-path "$MERGED" -- "$LOCAL" "$REMOTE"'
  git config --global difftool.prompt false
  git config --global difftool.trustExitCode true
  git config --global mergetool.Kaleidoscope.cmd 'ksdiff --merge --output "$MERGED" --base "$BASE" -- "$LOCAL" --snapshot "$REMOTE" --snapshot'
  git config --global mergetool.trustExitCode true
  git config --global diff.tool Kaleidoscope
  git config --global merge.tool Kaleidoscope
elif which code >/dev/null 2>&1 ; then
  echo "◉ VS Code found. Configuring Git to use it."
  git config --global merge.tool vscode
  git config --global mergetool.vscode.cmd 'code --wait --merge $REMOTE $LOCAL $BASE $MERGED'
  git config --global diff.tool vscode
  git config --global difftool.vscode.cmd 'code --wait --diff $LOCAL $REMOTE'
fi

if which tmux >/dev/null 2>&1 ; then
  echo "◉ Setting up tmux..."
  tpm="$HOME/.tmux/plugins/tpm"
  if [ -e "$tpm" ]; then
    pushd "$tpm" >/dev/null
    git pull -q origin master
    popd >/dev/null
  else
    git clone -q https://github.com/tmux-plugins/tpm "$HOME/.tmux/plugins/tpm"
  fi
  $tpm/scripts/install_plugins.sh >/dev/null
  $tpm/scripts/clean_plugins.sh >/dev/null
  $tpm/scripts/update_plugin.sh >/dev/null
else
  echo "◉ Skipping tmux setup because tmux isn't installed."
fi

postinstall="$HOME/.postinstall"
if [ -e "$postinstall" ]; then
  echo "◉ Running post-install..."
  . "$postinstall"
else
  echo "◉ No post install script found. Optionally create one at $postinstall"
fi

if [ ! -e "$HOME/.zshlocal" ]; then
  color=$((22 + RANDOM % 209))
  cat <<EOF >"$HOME/.zshlocal"
__current_git_branch() {
  if git rev-parse --git-dir > /dev/null 2>&1; then
    echo " \$(git branch --show-current 2>/dev/null) "
  fi
}

__node_version() {
  echo "⬣ \$(node --version 2>/dev/null) "
}

# If you want a different color, run ~/bin/256-colors.sh and replace $color below:
colorprompt "38;5;$color" "\\\$(__current_git_branch)\\\$(__node_version)"
EOF
  echo "◉ Random prompt color is $color. Edit $HOME/.zshlocal to change it."
fi

echo "✓ Done."

} # This ensures the entire script is downloaded.
