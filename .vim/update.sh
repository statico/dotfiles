#!/usr/bin/env bash
#
# Usage: ./update.sh [pattern]
#
# Specify [pattern] to update only repos that match the pattern.

repos=(

  airblade/vim-gitgutter
  alampros/vim-styled-jsx
  altercation/vim-colors-solarized
  ap/vim-css-color
  docunext/closetag.vim
  ervandew/supertab
  haya14busa/incsearch.vim
  itchyny/lightline.vim
  junegunn/fzf.vim
  junegunn/goyo.vim
  justinmk/vim-sneak
  octref/rootignore
  rking/ag.vim
  scrooloose/nerdtree
  sheerun/vim-polyglot
  statico/vim-inform7
  tomasr/molokai
  tpope/vim-commentary
  tpope/vim-fugitive
  tpope/vim-markdown
  tpope/vim-pathogen
  tpope/vim-rhubarb
  tpope/vim-sleuth
  tpope/vim-surround
  tpope/vim-unimpaired
  vim-scripts/bufkill.vim
  w0rp/ale
  wellle/targets.vim
  yssl/QFEnter

)

set -e
dir=~/.dotfiles/.vim/bundle
mkdir -p $dir

for repo in ${repos[@]}; do
  if [ -n "$1" ]; then
    if ! (echo "$repo" | grep -i "$1" &>/dev/null) ; then
      continue
    fi
  fi
  dest="$dir/$(basename $repo | sed -e 's/\.git$//')"
  rm -rf $dest
  echo "· Cloning $repo"
  git clone --depth=1 -q https://github.com/$repo $dest
  rm -rf $dest/.git
done
