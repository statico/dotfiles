Vim plugin for Handlebars
=========================

Deprecation
-----------

**Important**: this plugin is deprecated, you should use [mustache/vim-mode](https://github.com/mustache/vim-mode) instead!

About
-----

This plugin is here to help you editing
[Handlebars](http://www.handlebarsjs.com/) files.


Install
-------

**Install for pathogen**

    cd ~/.vim/
    git submodule add git://github.com/nono/vim-handlebars.git bundle/handlebars
    vim bundle/handlebars/example.handlebars

**Manually Install**

    git clone git://github.com/nono/vim-handlebars.git
    cd vim-handlebars
    cp -R ftdetect/* ~/.vim/ftdetect/
    cp -R ftplugin/* ~/.vim/ftplugin/
    cp -R syntax/* ~/.vim/syntax/
    vim example.handlebars


Credits
-------

Handlebars is the work of [Yehuda Katz](https://github.com/wycats).

This plugin was strongly inspired by [mustache.vim](https://github.com/juvenn/mustache.vim).

♡2011 by Bruno Michel. Copying is an act of love. Please copy and share.
