#!/usr/bin/env bash

{ # This ensures the entire script is downloaded.

set -e

basedir=$HOME/.dotfiles
bindir=$HOME/bin
repourl=git://github.com/statico/dotfiles.git

function symlink() {
  src=$1
  dest=$2

  if [ -e $dest ]; then
    if [ -L $dest ]; then
      # Already symlinked -- I'll assume correctly.
      return
    else
      # Rename files with a ".old" extension.
      echo "$dest already exists, renaming to $dest.old"
      backup=$dest.old
      if [ -e $backup ]; then
        echo "Error: $backup already exists. Please delete or rename it."
        exit 1
      fi
      mv -v $dest $backup
    fi
  fi
  ln -sf $src $dest
}

if ! which git >/dev/null ; then
  echo "Error: git is not installed"
  exit 1
fi

if [ -d $basedir/.git ]; then
  echo "Updating dotfiles using existing git..."
  cd $basedir
  git pull --quiet --rebase origin master
else
  echo "Checking out dotfiles using git..."
  rm -rf $basedir
  git clone --quiet --depth=1 $repourl $basedir
fi

cd $basedir

echo "Creating symlinks..."
for path in .* ; do
  case $path in
    .|..|.git)
      continue
      ;;
    *)
      symlink $basedir/$path $HOME/$path
      ;;
  esac
done
symlink $basedir/.vim/vimrc $HOME/.vimrc
symlink $basedir/.vim/gvimrc $HOME/.gvimrc

echo "Adding executables to ~/bin/..."
mkdir -p $bindir
for path in bin/* ; do
  symlink $basedir/$path $bindir/$(basename $path)
done

echo "Setting up vim plugins..."
.vim/update.sh

echo "Setting up git..."
cp $basedir/.gitconfig.base $HOME/.gitconfig

postinstall=$HOME/.postinstall
if [ -e $postinstall ]; then
  echo "Running post-install..."
  . $postinstall
else
  echo "No post install script found. Optionally create one at $postinstall"
fi

echo "Done."

} # This ensures the entire script is downloaded.
