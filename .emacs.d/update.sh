#!/bin/bash -ex
#
# Updates Pymacs, rope, ropemode and ropemacs.
#

pylib=~/.emacs.d/pylib
tmp=/tmp/$LOGNAME-emacs-update

mkdir $tmp
pushd $tmp

echo "Updating Pymacs"
rm -rf $pylib/Pymacs
curl -O http://pymacs.progiciels-bpi.ca/archives/Pymacs.tar.gz
tar zxf Pymacs.tar.gz
pushd Pymacs-*
make
cp -Rv build/lib/Pymacs $pylib
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