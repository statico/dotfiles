Ian's Vim Configuration
-----------------------

[vimrc](.vim/vimrc) and [gvimrc](.vim/gvimrc) are my configurations, but they also source `~/.vimlocal` and `~/.gvimlocal` respectively which allows per-host customization.

All third-party plugins are now managed by Pathogen and placed into [bundle/](.vim/bundle/). Some good intros are [here](http://tammersaleh.com/posts/the-modern-vim-config-with-pathogen) and [here](http://nvie.com/posts/how-i-boosted-my-vim/). Packages are updated using an [update.sh](.vim/update.sh) script.

[syntax/](.vim/syntax/) has some legacy syntax files I wrote. [chars](.vim/chars) is a test file used to make sure Unicode is working.
