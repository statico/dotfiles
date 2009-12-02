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

function die() {
    echo $*
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
            curl -s $tarball >$tempfile
        elif has wget; then
            wget -q -O $tempfile $tarball
        else:
            die "Can't download tarball."
        fi
        tar --strip-components 1 -zxf $tempfile
    fi
fi




