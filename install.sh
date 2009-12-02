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
gitbase=git://github.com/statico/dotfiles.git
tarball=http://github.com/statico/dotfiles/tarball/master

function has() {
    return $( which $1 >/dev/null )
}

function note() {
    echo "[32;1m$*[0m"
}

function warn() {
    echo "[31;1m$*[0m"
}

function die() {
    warn $*
    exit 1
}

mkdir -p $basedir
cd $basedir

if [ ! -e $basedir ]; then
    # .dotfiles directory needs to be installed. Try downloading first with
    # git, then use tarballs.
    if has git; then
        git clone $gitbase $basedir
    else
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

# Symlink all files in place.
for path in $( find . ); do
    if [ -d $path ]; then
        mkdir -v -p "$HOME/$( dirname $path )"
    else
        src=$basedir/$path
        dest=$HOME/$path

        if [ -f $dest ]; then
            # Rename files with a ".old" extension.
            warn "$dest file already exists, renaming to $dest.old"
            backup=$dest.old
            if [ -e $backup ]; then
                die "$backup already exists. Aborting."
            fi
            mv -v $dest $backup
        fi

        ln -s -f $src $dest
    fi
done

note "Done."
