#!/bin/bash -e
#
# Updates Vim plugins.
#

cd ~/.dotfiles

vimdir=.vim
bundledir=$vimdir/bundle
tmp=/tmp/$LOGNAME-vim-update

# URLS --------------------------------------------------------------------

repos=(
  git://git.wincent.com/command-t.git
  https://github.com/altercation/vim-colors-solarized.git
  https://github.com/fholgado/minibufexpl.vim.git
  https://github.com/kchmck/vim-coffee-script.git
  https://github.com/msanders/snipmate.vim.git
  https://github.com/scrooloose/nerdtree.git
  https://github.com/tpope/vim-fugitive.git
  https://github.com/tpope/vim-haml.git
  https://github.com/tpope/vim-markdown.git
  https://github.com/tpope/vim-pathogen.git
  https://github.com/tpope/vim-surround.git
  https://github.com/vim-scripts/Railscasts-Theme-GUIand256color.git
  https://github.com/vim-scripts/ZenCoding.vim.git
  https://github.com/vim-scripts/moria.git
  )

other=(
  'vim-fuzzyfinder;https://bitbucket.org/ns9tks/vim-fuzzyfinder/get/tip.zip'
  'zenburn/color;http://slinky.imukuppi.org/zenburn/zenburn.vim'
  'L9;https://bitbucket.org/ns9tks/vim-l9/get/tip.zip'
  )

case "$1" in

  # GIT -----------------------------------------------------------------
  repos)
    set -x

    for url in ${repos[@]}; do
      dest="$bundledir/$(basename $url | sed -e 's/\.git$//')"
      git submodule add $url $dest || true # Argh.
      git config submodule.$dest.ignore dirty
    done

    git submodule update --init $bundledir
    git submodule update --rebase $bundledir
    ;;

  # TARBALLS AND SINGLE FILES -------------------------------------------
  other)
    set -x
    rm -rf $tmp
    mkdir $tmp
    pushd $tmp

    for pair in ${other[@]}; do
      parts=($(echo $pair | tr ';' '\n'))
      name=${parts[0]}
      url=${parts[1]}
      dest=$bundledir/$name
      f=download

      rm -rf $dest $f

      curl -L $url >$f
      if echo $url | egrep '.vim$'; then
        # Single file
        mkdir -p $dest
        pushd $dest
        curl -OL $url
        popd
      elif echo $url | egrep '.zip$'; then
        # Zip archive
        unzip $f -d $name
        mkdir -p $dest
        mv $name/*/* $dest
        rm -rf $name
      else
        # Tarball
        echo TODO
      fi

    done

    popd
    rm -rf $tmp
    ;;

  # COMPILING -----------------------------------------------------------
  compile)
    for dir in $bundledir/*/Rakefile; do
      pushd "$(dirname $dir)"
      rake make || true
      popd
    done
    ;;

  # HELP ----------------------------------------------------------------
  *)
    set +x
    echo
    echo "Usage: $0 <section>"
    echo "...where section is one of:"
    egrep '\w\)$' $0 | sed -e 's/)//'
    exit 1

esac
