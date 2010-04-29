"
" manual filetype assignment
"
augroup filetypedetect

au BufNewFile,BufRead *.as      setf actionscript
au BufNewFile,BufRead *.bb      setf xdefaults
au BufNewFile,BufRead *.csv     setf csv
au BufNewFile,BufRead *.flr     setf actionscript
au BufNewFile,BufRead *.ini     setf conf
au BufNewFile,BufRead *.input   setf gnuplot
au BufNewFile,BufRead *.md      setlocal ft=md nolist spell
au BufNewFile,BufRead *.plist   setf xml
au BufNewFile,BufRead *.rb      setlocal noai
au BufNewFile,BufRead *.rhtm    setf eruby
au BufNewFile,BufRead *.rhtml   setf eruby
au BufNewFile,BufRead *.rxml    setf ruby
au BufNewFile,BufRead *.sch     setf scheme
au BufNewFile,BufRead *.scm     setf scheme
au BufNewFile,BufRead *.ss      setf scheme
au BufNewFile,BufRead *.t       setf perl
au BufNewFile,BufRead *.ttml    setf xml
au BufNewFile,BufRead *.xhtml   setf xml
au BufNewFile,BufRead *.xml     setlocal ft=xml  ts=2 sw=2 et
au BufNewFile,BufRead *.xps     setf perl
au BufNewFile,BufRead *.xsl     setlocal ft=xslt ts=2 sw=2 et
au BufNewFile,BufRead *.yaml    setlocal ft=yaml
au BufNewFile,BufRead *.yml     setlocal ft=yaml
au BufNewFile,BufRead *.zsh     setf zsh
au BufNewFile,BufRead *conkyrc setf conkyrc
au BufNewFile,BufRead *templates/*.html setf htmldjango
au BufNewFile,BufRead .vimlocal,.gvimlocal setf vim
au BufNewFile,BufRead .zshlocal setf zsh
au BufNewFile,BufRead .gitconfig* setf gitconfig
au BufNewFile,BufRead /etc/apache*/* setf apache
au BufNewFile,BufRead /tmp/crontab* setf crontab
au BufNewFile,BufRead /tmp/mutt-* setlocal spell nolist nohlsearch
au BufNewFile,BufRead /tmp/sql* setf sql
au BufNewFile,BufRead /usr/share/zsh/*/functions/* setf zsh
au BufNewFile,BufRead COMMIT_EDITMSG setlocal nolist nonumber
au BufNewFile,BufRead Makefile setlocal nolist
au BufNewFile,BufRead pf.conf setf pf
au BufNewFile,BufRead pf.conf.* setf pf

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
