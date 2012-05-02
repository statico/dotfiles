#!/bin/bash -e
#
# Updates Vim plugins.
#

cd ~/.dotfiles

vimdir=$PWD/.vim
bundledir=$vimdir/bundle
tmp=/tmp/$LOGNAME-vim-update
me=.vim/update.sh

if [ -n "$INSECURE" ]; then
  curl='curl --insecure'
  export GIT_SSL_NO_VERIFY=true
else
  curl='curl'
fi

# URLS --------------------------------------------------------------------

# This is a list of all plugins which are available via Git repos.
repos=(
  git://git.wincent.com/command-t.git
  https://github.com/Lokaltog/vim-powerline.git
  https://github.com/altercation/vim-colors-solarized.git
  https://github.com/docunext/closetag.vim.git
  https://github.com/hced/bufkill-vim.git
  https://github.com/kchmck/vim-coffee-script.git
  https://github.com/michaeljsmith/vim-indent-object
  https://github.com/msanders/snipmate.vim.git
  https://github.com/pangloss/vim-javascript.git
  https://github.com/plasticboy/vim-markdown.git
  https://github.com/scrooloose/nerdtree.git
  https://github.com/scrooloose/syntastic.git
  https://github.com/tpope/vim-fugitive.git
  https://github.com/tpope/vim-haml.git
  https://github.com/tpope/vim-pathogen.git
  https://github.com/tpope/vim-ragtag.git
  https://github.com/tpope/vim-surround.git
  https://github.com/vim-scripts/Railscasts-Theme-GUIand256color.git
  https://github.com/vim-scripts/ZenCoding.vim.git
  https://github.com/vim-scripts/keepcase.vim.git
  https://github.com/vim-scripts/lighttpd-syntax.git
  https://github.com/vim-scripts/moria.git
  https://github.com/vim-scripts/oceandeep.git
  https://github.com/wavded/vim-stylus.git
  )

# Here's a list of everything else to download in the format
# <destination>;<url>
other=(
  'vim-fuzzyfinder;https://bitbucket.org/ns9tks/vim-fuzzyfinder/get/tip.zip'
  'zenburn/colors;http://slinky.imukuppi.org/zenburn/zenburn.vim'
  'L9;https://bitbucket.org/ns9tks/vim-l9/get/tip.zip'
  )

case "$1" in

  # GIT -----------------------------------------------------------------
  repos)
    set -x
    mkdir -p $bundledir
    for url in ${repos[@]}; do
      dest="$bundledir/$(basename $url | sed -e 's/\.git$//')"
      rm -rf $dest
      git clone $url $dest
      rm -rf $dest/.git
    done
    ;;

  # TARBALLS AND SINGLE FILES -------------------------------------------
  other)
    set -x
    mkdir -p $bundledir
    rm -rf $tmp
    mkdir $tmp
    pushd $tmp

    for pair in ${other[@]}; do
      parts=($(echo $pair | tr ';' '\n'))
      name=${parts[0]}
      url=${parts[1]}
      dest=$bundledir/$name

      rm -rf $dest

      if echo $url | egrep '.vim$'; then
        # For single files, create the destination directory and download the
        # file there. The filename.
        mkdir -p $dest
        pushd $dest
        $curl -OL $url
        popd

      elif echo $url | egrep '.zip$'; then
        # Zip archives from VCS tend to have an annoying outer wrapper
        # directory, so unpacking them into their own directory first makes it
        # easy to remove the wrapper.
        f=download.zip
        $curl -L $url >$f
        unzip $f -d $name
        mkdir -p $dest
        mv $name/*/* $dest
        rm -rf $name $f

      else
        # Tarballs: TODO
        echo TODO
      fi

    done

    popd
    rm -rf $tmp
    ;;

  # COMPILING -----------------------------------------------------------
  compile)
    # Some plugins, particularly Command-T, need to be compiled.
    for dir in $bundledir/*/Rakefile; do
      pushd "$(dirname $dir)"
      rake make || true
      popd
    done
    echo "Compile OK"
    ;;

  # HELP ----------------------------------------------------------------

  all)
    $me repos
    $me other
    $me compile
    echo
    echo "Update OK"
    ;;

  *)
    set +x
    echo
    echo "Usage: $0 <section>"
    echo "...where section is one of:"
    grep -E '\w\)$' $me | sed -e 's/)//'
    exit 1

esac
