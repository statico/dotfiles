# How to use CoffeeTags with [TagBar ](https://github.com/majutsushi/tagbar)

## Config types

CoffeeTags can work in 2 modes:

- tags only for functions (default)
- tags for functions and objects containing them

Second mode is activated by adding --include-vars to command line arguments

#   CoffeeTags + TagBar + Vim

## Config in  vimrc

You can add the config to your .vimrc (making sure that the old one is removed)
by:

  coffeetags --vim-conf >> ~/.vimrc

or (for 2nd mode)

  coffeetags --include-vars --vim-conf >> ~/.vimrc


## Config as a filetype plugin

You can generate a special filetype plugin and tagbar will use that
automatically.

This option is preferable if you want to keep your vimrc short.

  coffeetags --vim-conf > ~/vim/ftplugin/coffee/tagbar-coffee.vim
  coffeetags [--include-vars] --vim-conf > ~/vim/ftplugin/coffee/tagbar-coffee.vim

or if you're using pathogen

  coffeetags [--include-vars] --vim-conf > ~/vim/bundle/coffeetags/ftplugin/coffee/tagbar-coffee.vim
  coffeetags --vim-conf > ~/vim/bundle/coffeetags/ftplugin/coffee/tagbar-coffee.vim

