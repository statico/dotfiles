#!/usr/bin/env zsh
{ # This ensures the entire script is downloaded.

set -eo pipefail

basedir="$HOME/.dotfiles"
bindir="$HOME/bin"
repourl="https://github.com/statico/dotfiles.git"

function symlink() {
  src="$1"
  dest="$2"

  if [ -e "$dest" ]; then
    if [ -L "$dest" ]; then
      if [ ! -e "$dest" ]; then
        echo "Removing broken symlink at $dest"
        rm "$dest"
      else
        # Already symlinked -- I'll assume correctly.
        return 0
      fi
    else
      # Rename files with a ".old" extension.
      echo "$dest already exists, renaming to $dest.old"
      backup="$dest.old"
      if [ -e "$backup" ]; then
        echo "Error: "$backup" already exists. Please delete or rename it."
        exit 1
      fi
      mv "$dest" "$backup"
    fi
  fi
  ln -sf "$src" "$dest"
}

if ! which git >/dev/null ; then
  echo "Error: git is not installed"
  exit 1
fi

if [ -d "$basedir/.git" ]; then
  echo "Updating dotfiles using existing git..."
  cd "$basedir"
  git pull --quiet --rebase origin main || exit 1
else
  echo "Checking out dotfiles using git..."
  rm -rf "$basedir"
  git clone --quiet --depth=1 "$repourl" "$basedir"
fi

cd "$basedir"

echo "Updating common Zsh completions..."
rm -rf .zsh-completions ~/.zcompdump
git clone --quiet --depth=1 https://github.com/zsh-users/zsh-completions .zsh-completions

echo "Creating symlinks..."
for item in .* ; do
  case "$item" in
    .|..|.git)
      continue
      ;;
    *)
      symlink "$basedir/$item" "$HOME/$item"
      ;;
  esac
done
symlink "$basedir/.vim/vimrc" "$HOME/.vimrc"
symlink "$basedir/.vim/gvimrc" "$HOME/.gvimrc"

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

echo "Adding executables to ~/bin/..."
mkdir -p "$bindir"
for item in bin/* ; do
  symlink "$basedir/$item" "$bindir/$(basename $item)"
done

if [ -n "$VSCODE_REMOTE_CONTAINERS_SESSION" ]; then
  # We must be setting up a VS Code remote dev container, so I probably won't use Vim.
  echo "VS Code remote environment detected. Skipping Vim setup."
else
  echo "Setting up vim plugins..."
  .vim/update.sh
fi

echo "Setting up git..."
if [ -n "$VSCODE_REMOTE_CONTAINERS_SESSION" ]; then
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
  git lfs install
fi
if which code >/dev/null 2>&1 ; then
  echo "VS Code found. Configuring Git to use it."
  git config --global diff.tool vscode
  git config --global difftool.vscode.cmd 'code --wait --diff $LOCAL $REMOTE'
  git config --global merge.tool vscode
  git config --global mergetool.vscode.cmd 'code --wait $MERGED'
fi

if which tmux >/dev/null 2>&1 ; then
  echo "Setting up tmux..."
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
  echo "Skipping tmux setup because tmux isn't installed."
fi

postinstall="$HOME/.postinstall"
if [ -e "$postinstall" ]; then
  echo "Running post-install..."
  . "$postinstall"
else
  echo "No post install script found. Optionally create one at $postinstall"
fi

if [ ! -e "$HOME/.zshlocal" ]; then
  color=$((22 + RANDOM % 209))
  echo -e "# If you want a different color, run ~/bin/256-colors.sh and replace $color below:\ncolorprompt \"38;5;$color\"" >"$HOME/.zshlocal"
  echo "Chose a random prompt color. Edit $HOME/.zshlocal to change it."
fi

echo "Done."

} # This ensures the entire script is downloaded.
