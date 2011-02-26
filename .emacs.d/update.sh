#!/bin/bash -ex
#
# Updates Pymacs, rope, ropemode and ropemacs.
# Updates all sorts of things.
#

pylib=~/.emacs.d/python
vendordir=~/.emacs.d/vendor
pylib=~/.emacs.d/python
tmp=/tmp/$LOGNAME-emacs-update

echo "Updating single files"
pushd $vendordir

curl -O http://google-styleguide.googlecode.com/svn/trunk/google-c-style.el
curl -O http://hg.rooijan.za.net/addons/raw-file/tip/ack-emacs.el
curl -O http://hg.rooijan.za.net/addons/raw-file/tip/ack.el
curl -O http://jblevins.org/projects/markdown-mode/markdown-mode.el
curl -O http://nschum.de/src/emacs/highlight-symbol/highlight-symbol.el
curl -O http://www.dr-qubit.org/undo-tree/undo-tree.el
curl -O http://www.emacswiki.org/emacs/download/browse-kill-ring.el
curl -O http://www.emacswiki.org/emacs/download/centered-cursor-mode.el
curl -O http://www.emacswiki.org/emacs/download/frame-cmds.el
curl -O http://www.emacswiki.org/emacs/download/frame-fns.el
curl -O http://www.emacswiki.org/emacs/download/highlight-current-line.el
curl -O http://www.emacswiki.org/emacs/download/vimpulse.el
curl -O http://www.emacswiki.org/emacs/download/zoom-frm.el
curl -O https://github.com/defunkt/coffee-mode/raw/master/coffee-mode.el
curl -O https://github.com/yoshiki/yaml-mode/raw/master/yaml-mode.el

cd color-theme/themes
curl -O http://github.com/olegshaldybin/color-theme-railscasts/raw/master/color-theme-railscasts.el

popd
rm -rf $tmp
mkdir $tmp
pushd $tmp

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
rm -rf $f

echo "Updating Pymacs"
rm -rf $pylib/Pymacs
curl -O http://pymacs.progiciels-bpi.ca/archives/Pymacs.tar.gz
tar zxf Pymacs.tar.gz
pushd Pymacs-*
make
cp -Rv build/lib/Pymacs $pylib
mkdir -p $vendordir/pymacs
cp pymacs.el $vendordir/pymacs
popd
rm -rf Pymacs-* Pymacs.tar.gz

echo "Updating rope"
rm -rf $pylib/rope
curl -O http://bitbucket.org/agr/rope/get/tip.gz
tar zxf tip.gz --strip-components 1 rope/rope
cp -Rv rope $pylib
rm tip.gz

echo "Updating ropemode"
curl -O http://bitbucket.org/agr/ropemode/get/tip.gz
tar zxf tip.gz
pushd ropemode
python setup.py build
cp -Rv build/lib/ropemode $pylib
popd
rm -rf ropemode tip.gz

echo "Updating ropemacs"
rm -rf $pylib/ropemacs
curl -O http://bitbucket.org/agr/ropemacs/get/tip.gz
tar zxf tip.gz
pushd ropemacs
python setup.py build
cp -Rv build/lib/ropemacs $pylib
popd
rm -rf ropemacs tip.gz

popd
rm -rf $tmp
