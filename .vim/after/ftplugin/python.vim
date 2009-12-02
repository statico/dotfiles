" John Reese says this is as close to Google style as we can get for Python
set indentexpr= cindent cino=(0

" ropevim
function! TabWrapperRope()
  if strpart(getline('.'), 0, col('.')-1) =~ '^\s*$'
    return "\<Tab>"
  else
    return "\<C-R>=RopeCodeAssistInsertMode()\<CR>"
  endif
endfunction
imap <Tab> <C-R>=TabWrapperRope()<CR>

nmap <Esc>C <Esc>:call RopeShowCalltip()<CR>
nmap <Esc>F <Esc>:call RopeFindOccurences()<CR>
nmap <Esc>H <Esc>:call RopeShowDoc()<CR>
nmap <Esc>M <Esc>:call RopeMove()<CR>
nmap <Esc>N <Esc>:call RopeRename()<CR>
nmap <Esc>O <Esc>:call RopeOrganizeImports()<CR>
nmap <Esc>R <Esc>:call RopeRedo()<CR>
nmap <Esc>T <Esc>:call RopeExtractMethod()<CR>
nmap <Esc>U <Esc>:call RopeUndo()<CR>
nmap <Esc>V <Esc>:call RopeExtractVariable()<CR>


