#!/bin/bash -ex
#
# Updates Pymacs, rope, ropemode and ropemacs.
# Updates all sorts of things.
#

pylib=~/.emacs.d/python
vendordir=~/.emacs.d/vendor
pylib=~/.emacs.d/python
tmp=/tmp/$LOGNAME-emacs-update

rm -rf $tmp
mkdir $tmp
pushd $tmp

function unpack () {
  rm -rf $1
  mkdir $1
  pushd $1
  curl -L $2 >$1.tar.gz
  tar zxf $1.tar.gz
}

case "$1" in

  smallslow)
    echo "Updating single rate-limited files"
    pushd $vendordir
    urls=(
      http://www.emacswiki.org/emacs/download/anything.el
      http://www.emacswiki.org/emacs/download/anything-config.el
      http://www.emacswiki.org/emacs/download/anything-match-plugin.el
      http://www.emacswiki.org/emacs/download/browse-kill-ring.el
      http://www.emacswiki.org/emacs/download/centered-cursor-mode.el
      http://www.emacswiki.org/emacs/download/flymake-cursor.el
      http://www.emacswiki.org/emacs/download/frame-cmds.el
      http://www.emacswiki.org/emacs/download/frame-fns.el
      http://www.emacswiki.org/emacs/download/highlight-current-line.el
      http://www.emacswiki.org/emacs/download/sr-speedbar.el
      http://www.emacswiki.org/emacs/download/tiling.el
      http://www.emacswiki.org/emacs/download/vimpulse.el
      http://www.emacswiki.org/emacs/download/zoom-frm.el
    )
    for url in ${urls[@]} ; do
      curl -sOL $url
      sleep 2
    done
    wait
    popd
    ;;

  small)
    echo "Updating single files"
    pushd $vendordir
    urls=(
      http://google-styleguide.googlecode.com/svn/trunk/google-c-style.el
      http://hg.rooijan.za.net/addons/raw-file/tip/ack-emacs.el
      http://hg.rooijan.za.net/addons/raw-file/tip/ack.el
      #http://hg.piranha.org.ua/project-root/raw-file/tip/project-root.el
      #http://hg.piranha.org.ua/project-root/raw-file/tip/find-cmd.el
      http://jblevins.org/projects/markdown-mode/markdown-mode.el
      http://nschum.de/src/emacs/highlight-symbol/highlight-symbol.el
      http://www.dr-qubit.org/undo-tree/undo-tree.el
      http://emacs-nav.googlecode.com/hg/nav.el
      https://raw.github.com/voins/mo-git-blame/master/mo-git-blame.el
      https://raw.github.com/yoshiki/yaml-mode/master/yaml-mode.el
      https://raw.github.com/nex3/haml-mode/master/haml-mode.el
      https://raw.github.com/defunkt/coffee-mode/master/coffee-mode.el
      https://raw.github.com/nex3/sass-mode/master/sass-mode.el
    )
    for url in ${urls[@]} ; do
      curl -sOL $url
    done
    wait
    popd
    ;;

  themes)
    pushd $vendordir
    cd color-theme/themes
    curl -OL https://github.com/olegshaldybin/color-theme-railscasts/raw/master/color-theme-railscasts.el
    curl -OL http://www.emacswiki.org/emacs/download/color-theme-tango.el
    popd

    unpack solarized https://github.com/sellout/emacs-color-theme-solarized/tarball/master
    cd sellout-emacs-color-theme*
    cp -v *.el $vendordir/color-theme/themes
    popd

    unpack tomorrow https://github.com/ChrisKempson/Tomorrow-Theme/tarball/master
    cd ChrisKempson-Tomorrow*
    cp -v GNU\ Emacs/*.el $vendordir/color-theme/themes
    popd

    ;;

  evernote)
    echo "Updating evernote"
    f=evernote
    dest=$vendordir/evernote-mode
    rm -rf $f
    mkdir $f
    pushd $f
    curl -L http://emacs-evernote-mode.googlecode.com/files/evernote-mode-0_41.zip >$f.zip
    unzip $f.zip
    cp -r evernote-mode* $dest
    cd $dest/ruby
    ruby setup.rb
    popd
    ;;

  jshint)
    echo "Updating jshint"
    f=jshint
    rm -rf $f
    mkdir $f
    pushd $f
    curl -L https://github.com/daleharvey/jshint-mode/tarball/master >$f.tar.gz
    tar zxf $f.tar.gz
    cp -r daleharvey-jshint-mode* $vendordir/jshint-mode
    popd
    ;;

  magit)
    echo "Updating magit"
    f=magit
    rm -rf $f
    mkdir $f
    pushd $f
    curl -L https://github.com/philjackson/magit/tarball/master >$f.tar.gz
    tar zxf $f.tar.gz
    cd philjackson-magit*
    cp -v magit*.el $vendordir/
    cp -v magit.texi ~/.dotfiles/info/
    popd
    ;;

  ethan)
    echo "Updating ethan-whitespace"
    f=ethan-wspace
    rm -rf $f
    mkdir $f
    pushd $f
    curl -L https://github.com/glasserc/ethan-wspace/tarball/master >$f.tar.gz
    tar zxf $f.tar.gz
    cd glasserc-ethan-wspace*
    rm -rf $vendordir/$f
    cp -Rv lisp $vendordir/$f
    popd
    ;;

  yasnippet)
    echo "Updating yasnippet"
    f=yasnippet
    rm -rf $f
    mkdir $f
    pushd $f
    curl -L http://yasnippet.googlecode.com/files/yasnippet-0.6.1c.tar.bz2 >$f.tar.gz
    tar zxf $f.tar.gz
    cd $f*
    dest=$vendordir/$f
    rm -rf $dest
    mkdir -p $dest
    mv * $dest
    popd
    ;;

  pymacs)
    echo "Updating Pymacs"
    rm -rf $pylib/Pymacs
    curl -L https://github.com/pinard/Pymacs/tarball/master >Pymacs.tar.gz
    tar zxf Pymacs.tar.gz
    pushd pinard-Pymacs-*
    make
    cp -Rv build/lib/Pymacs $pylib
    mkdir -p $vendordir/pymacs
    cp pymacs.el $vendordir/pymacs
    popd
    ;;

  rope)
    echo "Updating rope"
    rm -rf $pylib/rope
    curl -LO http://bitbucket.org/agr/rope/get/tip.gz
    tar zxf tip.gz --strip-components 1 \*/rope
    cp -Rv rope $pylib
    rm tip.gz

    echo "Updating ropemode"
    curl -LO http://bitbucket.org/agr/ropemode/get/tip.gz
    tar zxf tip.gz
    pushd agr-ropemode-*
    python setup.py build
    cp -Rv build/lib/ropemode $pylib
    popd
    rm -v tip.gz

    echo "Updating ropemacs"
    rm -rf $pylib/ropemacs
    curl -LO http://bitbucket.org/agr/ropemacs/get/tip.gz
    tar zxf tip.gz
    pushd agr-ropemacs-*
    python setup.py build
    cp -Rv build/lib/ropemacs $pylib
    popd
    rm -v tip.gz
    ;;

  *)
    popd
    set +x
    echo
    echo "Usage: $0 <section>"
    echo "...where section is one of:"
    egrep '\w\)$' $0 | sed -e 's/)//'
    exit 1

esac

popd
rm -rf $tmp
