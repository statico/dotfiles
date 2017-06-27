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

  https://github.com/airblade/vim-gitgutter
  https://github.com/altercation/vim-colors-solarized
  https://github.com/ap/vim-css-color
  https://github.com/ctrlpvim/ctrlp.vim
  https://github.com/docunext/closetag.vim
  https://github.com/ervandew/supertab
  https://github.com/haya14busa/incsearch.vim
  https://github.com/junegunn/goyo.vim
  https://github.com/justinmk/vim-sneak
  https://github.com/rking/ag.vim
  https://github.com/scrooloose/nerdtree
  https://github.com/scrooloose/syntastic
  https://github.com/sheerun/vim-polyglot
  https://github.com/tomasr/molokai
  https://github.com/tpope/vim-commentary
  https://github.com/tpope/vim-fugitive
  https://github.com/tpope/vim-markdown
  https://github.com/tpope/vim-pathogen
  https://github.com/tpope/vim-sleuth
  https://github.com/tpope/vim-surround
  https://github.com/tpope/vim-unimpaired
  https://github.com/vim-airline/vim-airline
  https://github.com/vim-airline/vim-airline-themes
  https://github.com/vim-scripts/bufkill.vim
  https://github.com/wellle/targets.vim
  https://github.com/yssl/QFEnter

)

mkdir -p $bundledir

for url in ${repos[@]}; do
  if [ -n "$1" ]; then
    if ! (echo "$url" | grep -i "$1" &>/dev/null) ; then
      continue
    fi
  fi
  dest="$bundledir/$(basename $url | sed -e 's/\.git$//')"
  rm -rf $dest
  echo "Cloning $url"
  git clone --depth=1 -q $url $dest
  rm -rf $dest/.git
done
