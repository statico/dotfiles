#!/bin/sh -e
#
# Ian's dotfile installer. Usage:
#
#   curl http://github.com/statico/dotfiles/raw/master/install.sh | sh
#
# or:
#
#   ~/.dotfiles/install.sh
#

basedir=$HOME/.dotfiles
bindir=$HOME/bin
gitbase=git://github.com/statico/dotfiles.git
tarball=http://github.com/statico/dotfiles/tarball/master

function has() {
    return $( which $1 >/dev/null )
}

function note() {
    echo "[32;1m * [0m$*"
}

function warn() {
    echo "[31;1m * [0m$*"
}

function die() {
    warn $*
    exit 1
}

function install() {
    src=$1
    dest=$2
    if [ -e $dest ] && [ ! -s $dest ]; then
        # Rename files with a ".old" extension.
        warn "$dest file already exists, renaming to $dest.old"
        backup=$dest.old
        if [ -e $backup ]; then
            die "$backup already exists. Aborting."
        fi
        mv -v $dest $backup
    fi

    # Update existing or create new symlinks.
    ln -v -s -f $src $dest
}

mkdir -p $basedir
cd $basedir

if [ ! -e $basedir ]; then
    # .dotfiles directory needs to be installed. Try downloading first with
    # git, then use tarballs.
    if has git; then
        note "Cloning from git..."
        git clone $gitbase $basedir
    else
        note "Downloading tarball..."
        tempfile=TEMP.tar.gz
        if has curl; then
            curl $tarball >$tempfile
        elif has wget; then
            wget -O $tempfile $tarball
        else:
            die "Can't download tarball."
        fi
        tar --strip-components 1 -zxvf $tempfile
        rm -v $tempfile
    fi
fi

# Symlink all dotfiles.
note "Installing dotfiles..."
for path in .*; do
    [ $path == . ] && continue;
    [ $path == .. ] && continue;
    install $basedir/$path $HOME/$path
done

# Install any utilities into ~/bin.
note "Installing bin/ directory..."
mkdir -v -p $bindir
for path in ./bin/*; do
    install $basedir/$path $bindir/$path
done

note "Done."
