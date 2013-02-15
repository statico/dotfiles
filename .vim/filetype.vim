"
" manual filetype assignment
"

" Template syntax selector from Armin Ronacher (mitsuhiko). {{{1
fun! s:SelectHTML()
let n = 1
while n < 50 && n < line("$")
  " check for jinja
  if getline(n) =~ '{%\s*\(extends\|block\|macro\|set\|if\|for\|include\|trans\)\>'
    set ft=htmljinja
    return
  endif
  " check for django
  if getline(n) =~ '{%\s*\(extends\|block\|comment\|ssi\|if\|for\|blocktrans\)\>'
    set ft=htmldjango
    return
  endif
  " check for mako
    if getline(n) =~ '<%\(def\|inherit\)'
      set ft=mako
      return
    endif
    " check for genshi
    if getline(n) =~ 'xmlns:py\|py:\(match\|for\|if\|def\|strip\|xmlns\)'
      set ft=genshi
      return
    endif
    let n = n + 1
  endwhile
  " go with html
  set ft=html
endfun
" }}}

augroup filetypedetect

au BufNewFile,BufRead *.as      setlocal ft=actionscript nolist ts=4 sw=4 noet
au BufNewFile,BufRead *.bb      setf xdefaults
au BufNewFile,BufRead *.csv     setf csv
au BufNewFile,BufRead *.flr     setf actionscript
au BufNewFile,BufRead *.glsl    setf glsl
au BufNewFile,BufRead *.html,*.htm  call s:SelectHTML()
au BufNewFile,BufRead *.html    setlocal nocindent smartindent
au BufNewFile,BufRead *.ini     setf conf
au BufNewFile,BufRead *.input   setf gnuplot
au BufNewFile,BufRead *.json    setf javascript
au BufNewFile,BufRead *.less    setf less
au BufNewFile,BufRead *.less    setlocal nocindent smartindent
au BufNewFile,BufRead *.md      setlocal ft=markdown nolist spell
au BufNewFile,BufRead *.plist   setf xml
au BufNewFile,BufRead *.rb      setlocal noai
au BufNewFile,BufRead *.rhtm    setf eruby
au BufNewFile,BufRead *.rhtml   setf eruby
au BufNewFile,BufRead *.rxml    setf ruby
au BufNewFile,BufRead *.sass    setf sass
au BufNewFile,BufRead *.sch     setf scheme
au BufNewFile,BufRead *.scm     setf scheme
au BufNewFile,BufRead *.scss    setf scss
au BufNewFile,BufRead *.ss      setf scheme
au BufNewFile,BufRead *.t       setf perl
au BufNewFile,BufRead *.ttml    setf xml
au BufNewFile,BufRead *.xhtml   setf xml
au BufNewFile,BufRead *.xml     setlocal ft=xml  ts=2 sw=2 et
au BufNewFile,BufRead *.xps     setf perl
au BufNewFile,BufRead *.xsl     setlocal ft=xslt ts=2 sw=2 et
au BufNewFile,BufRead *.yaml    setlocal ft=yaml
au BufNewFile,BufRead *.yml     setlocal ft=yaml
au BufNewFile,BufRead *.zone    setlocal nolist ts=4 sw=4 noet
au BufNewFile,BufRead *.zsh     setf zsh
au BufNewFile,BufRead *conkyrc setf conkyrc
au BufNewFile,BufRead *templates/*.html setf htmldjango
au BufNewFile,BufRead .vimlocal,.gvimlocal setf vim
au BufNewFile,BufRead .zshlocal setf zsh
au BufNewFile,BufRead .git/config setlocal ft=gitconfig nolist ts=4 sw=4 noet
au BufNewFile,BufRead .gitconfig* setlocal ft=gitconfig nolist ts=4 sw=4 noet
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
