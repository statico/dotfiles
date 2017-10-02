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
  nathanaelkane/vim-indent-guides
  octref/rootignore
  qpkorr/vim-bufkill
  rking/ag.vim
  scrooloose/nerdtree
  sheerun/vim-polyglot
  statico/vim-inform7
  tomasr/molokai
  tpope/vim-commentary
  tpope/vim-endwise
  tpope/vim-eunuch
  tpope/vim-fugitive
  tpope/vim-pathogen
  tpope/vim-repeat
  tpope/vim-rhubarb
  tpope/vim-sleuth
  tpope/vim-surround
  tpope/vim-unimpaired
  w0rp/ale
  wellle/targets.vim

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
  plugin="$(basename $repo | sed -e 's/\.git$//')"
  [ "$plugin" = "vim-styled-jsx" ] && plugin="000-vim-styled-jsx" # https://goo.gl/tJVPja
  dest="$dir/$plugin"
  rm -rf $dest
  echo "· Cloning $repo"
  git clone --depth=1 -q https://github.com/$repo $dest
  rm -rf $dest/.git
done
