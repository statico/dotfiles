"
" manual filetype assignment
"

augroup filetypedetect

au BufNewFile,BufRead *.zsh     setf zsh
au BufNewFile,BufRead *.ss      setf scheme
au BufNewFile,BufRead *.ini     setf conf
au BufNewFile,BufRead *.xhtml   setf xml
au BufNewFile,BufRead *.ttml    setf xml
au BufNewFile,BufRead *.plist   setf xml
au BufNewFile,BufRead *.bb      setf xdefaults
au BufNewFile,BufRead *.xps     setf perl
au BufNewFile,BufRead *.t       setf perl
au BufNewFile,BufRead *.rhtm    setf eruby
au BufNewFile,BufRead *.rhtml   setf eruby
au BufNewFile,BufRead *.erb     setf eruby
au BufNewFile,BufRead *.input   setf gnuplot
au BufNewFile,BufRead *.pp      setf puppet

au BufNewFile,BufRead pf.conf   setf pf
au BufNewFile,BufRead pf.conf.* setf pf 

au BufNewFile,BufRead *.xml     set ft=xml  ts=2 sw=2 et
au BufNewFile,BufRead *.xsl     set ft=xslt ts=2 sw=2 et
au BufNewFile,BufRead *.yml     set ft=yaml
au BufNewFile,BufRead *.yaml    set ft=yaml

au BufNewFile,BufRead /tmp/crontab*     setf crontab
au BufNewFile,BufRead /tmp/sql*         setf sql
au BufNewFile,BufRead /usr/share/zsh/*/functions/*         setf zsh

" why in hell do I have to do this? only for mail!
au BufNewFile,BufRead /tmp/mutt-*       set nohlsearch

au BufNewFile,BufRead *.tt2
    \ if ( getline(1) . getline(2) . getline(3) =~ '<\chtml'
    \           && getline(1) . getline(2) . getline(3) !~ '<[%?]' )
    \   || getline(1) =~ '<!DOCTYPE HTML' |
    \   setf tt2html |
    \ else |
    \   setf tt2 |
    \ endif 

augroup END

runtime! ftdetect/*.vim
