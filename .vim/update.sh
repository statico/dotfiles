#!/bin/bash -e
#
# Updates Vim plugins.
#
# Update everything (long):
#
#   ./update.sh
#
# Update just one plugin that matches this pattern:
#
#   ./update.sh supertab
#

cd ~/.dotfiles

vimdir=$PWD/.vim
bundledir=$vimdir/bundle
tmp=/tmp/$LOGNAME-vim-update
me=.vim/update.sh

repos=(

  airblade/vim-gitgutter
  altercation/vim-colors-solarized
  ap/vim-css-color
  ctrlpvim/ctrlp.vim
  docunext/closetag.vim
  ervandew/supertab
  haya14busa/incsearch.vim
  junegunn/goyo.vim
  justinmk/vim-sneak
  rking/ag.vim
  scrooloose/nerdtree
  sheerun/vim-polyglot
  tomasr/molokai
  tpope/vim-commentary
  tpope/vim-fugitive
  tpope/vim-markdown
  tpope/vim-pathogen
  tpope/vim-sleuth
  tpope/vim-surround
  tpope/vim-unimpaired
  vim-airline/vim-airline
  vim-airline/vim-airline-themes
  vim-scripts/bufkill.vim
  w0rp/ale
  wellle/targets.vim
  yssl/QFEnter

)

mkdir -p $bundledir

for repo in ${repos[@]}; do
  if [ -n "$1" ]; then
    if ! (echo "$repo" | grep -i "$1" &>/dev/null) ; then
      continue
    fi
  fi
  dest="$bundledir/$(basename $repo | sed -e 's/\.git$//')"
  rm -rf $dest
  echo "Cloning $repo"
  git clone --depth=1 -q https://github.com/$repo $dest
  rm -rf $dest/.git
done
