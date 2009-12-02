"
" ruby-specific settings
"

" seems to be the common ruby tab
set sw=2 ts=2 sts=2

" from http://www.rubygarden.org/ruby?VimRubySupport
if !exists( "*EndToken" )
  function EndToken()
    let current_line = getline( '.' )
    let braces_at_end = '{\s*\(|\(,\|\s\|\w\)*|\s*\)\?$'
    if match( current_line, braces_at_end ) >= 0
      return '}'
    else
      return 'end'
    endif
  endfunction
endif

imap <C-CR> <ESC>:execute 'normal o' . EndToken()<CR>O
