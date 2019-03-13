#!/usr/bin/env bash
{ # This ensures the entire script is downloaded.

set -eo pipefail

basedir="$HOME/.dotfiles"
bindir="$HOME/bin"
repourl="git://github.com/statico/dotfiles.git"

function symlink() {
  src="$1"
  dest="$2"

  if [ -e "$dest" ]; then
    if [ -L "$dest" ]; then
      # Already symlinked -- I'll assume correctly.
      return
    else
      # Rename files with a ".old" extension.
      echo "$dest already exists, renaming to $dest.old"
      backup="$dest.old"
      if [ -e "$backup" ]; then
        echo "Error: "$backup" already exists. Please delete or rename it."
        exit 1
      fi
      mv -v "$dest" "$backup"
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
  git pull --quiet --rebase origin master || exit 1
else
  echo "Checking out dotfiles using git..."
  rm -rf "$basedir"
  git clone --quiet --depth=1 "$repourl" "$basedir"
fi

cd "$basedir"

echo "Creating symlinks..."
for path in .* ; do
  case "$path" in
    .|..|.git)
      continue
      ;;
    *)
      symlink "$basedir/$path" "$HOME/$path"
      ;;
  esac
done
symlink "$basedir/.vim/vimrc" "$HOME/.vimrc"
symlink "$basedir/.vim/gvimrc" "$HOME/.gvimrc"

if [ "$(uname -s)" = "Darwin" ]; then
  vscodepath="$HOME/Library/Application Support/Code/User"
else
  vscodepath="$HOME/.config/Code/User"
fi
mkdir -p "$vscodepath"
symlink "$basedir/.vscode.settings.json" "$vscodepath/settings.json"
symlink "$basedir/.vscode.keybindings.json" "$vscodepath/keybindings.json"

echo "Adding executables to ~/bin/..."
mkdir -p "$bindir"
for path in bin/* ; do
  symlink "$basedir/$path" "$bindir/$(basename $path)"
done

echo "Setting up vim plugins..."
.vim/update.sh

echo "Setting up git..."
cp "$basedir/.gitconfig.base" "$HOME/.gitconfig"
if which git-lfs >/dev/null 2>&1 ; then
  git lfs install
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

echo "Done."

} # This ensures the entire script is downloaded.
