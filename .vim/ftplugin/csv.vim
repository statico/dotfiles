" Filetype plugin for editing CSV files.
" Version 2008-11-05 from http://vim.wikia.com/wiki/csv
if v:version < 700 || exists('b:did_ftplugin')
  finish
endif
let b:did_ftplugin = 1

" Return number of characters (not bytes) in string.
function! s:CharLen(str)
  return strlen(substitute(a:str, '.', 'x', 'g'))
endfunction

" Display a warning message.
function! s:Warn(msg)
  echohl WarningMsg
  echo a:msg
  echohl NONE
endfunction

" Set or show column delimiter.
" Accept '\t' (2 characters: backslash, t) as the tab character.
function! s:Delimiter(delim)
  if a:delim != ''
    let want = a:delim == '\t' ? "\t" : a:delim
    if s:CharLen(want) != 1
      call s:Warn('Delimiter must be a single character')
      return
    endif
    let b:csv_delimiter = want
  endif
  let b:changed_done = -1
  let b:csv_column = 1
  silent call s:Highlight(b:csv_column)
  echo printf('Delimiter = "%s"', b:csv_delimiter == "\t" ? '\t' : strtrans(b:csv_delimiter))
endfunction
command! -buffer -nargs=? Delimiter call <SID>Delimiter('<args>')

" Get string containing delimiter (default ',') specified for current buffer.
" A command like ':let g:csv_delimiter = ";"' changes the default.
function! s:GetStr(id)
  if !exists('b:csv_delimiter') || b:csv_delimiter == ''
    if exists('g:csv_delimiter') && g:csv_delimiter != ''
      let b:csv_delimiter = g:csv_delimiter
    else
      let b:csv_delimiter = ','
    endif
  endif
  if !exists('b:csv_str')
    let b:csv_str = {'delim': ''}
  endif
  if b:csv_str['delim'] !=# b:csv_delimiter
    " Define strings using delimiter ',' then substitute if required.
    let b:csv_str['delim'] = ','
    let b:csv_str['numco'] = '\%(\%("\%([^"]\|""\)*"\)\|\%([^,"]*\)\)'
    let b:csv_str['expr1'] = '\%(\%("\zs\%([^"]\|""\)*\ze"\)\|\%(\zs[^,"]*\ze\)\)'
    let b:csv_str['expr2'] = '\%(\%(\zs"\%([^"]\|""\)*",\?\ze\)\|\%(\zs[^,"]*,\?\ze\)\)'
    let b:csv_str['expr3'] = '^\%(\%(\%("\%([^"]\|""\)*"\)\|\%([^,"]*\)\),\)\{'
    let b:csv_str['delco'] = ',$'
    let b:csv_str['sear1'] = '^\%(\%("\%([^,]\|""\)*\zs'
    let b:csv_str['sear2'] = '\ze\%([^,]\|""\)*"\)\|\%([^,"]*\zs'
    let b:csv_str['sear3'] = '\ze[^,"]*\)\)'
    let b:csv_str['sear4'] = '^\%(\%(\%("\%([^"]\|""\)*"\)\|\%([^,"]*\)\),\)\{'
    let b:csv_str['sear5'] = '}\%(\%("\%([^,]\|""\)*\zs'
    if b:csv_delimiter != ','
      for key in keys(b:csv_str)
        let b:csv_str[key] = substitute(b:csv_str[key], ',', b:csv_delimiter, 'g')
      endfor
    endif
  endif
  return b:csv_str[a:id]
endfunction

" Get the number of columns (maximum of number in first and last three
" lines; at least one of them should contain typical csv data).
function! s:GetNumCols()
  let b:csv_max_col = 1
  for l in [1, 2, 3, line('$')-2, line('$')-1, line('$')]
    " Determine number of columns by counting the (unescaped) delimiters.
    " Note: The regexp may also return unbalanced ", so filter out anything
    " which isn't a delimiter in the second pass.
    let c = s:CharLen(substitute(substitute(getline(l), s:GetStr('numco'), '', 'g'), '"', '', 'g')) + 1
    if b:csv_max_col < c
      let b:csv_max_col = c
    endif
  endfor
  if b:csv_max_col <= 1
    let b:csv_max_col = 999
    call s:Warn('No delimiter-separated columns were detected.')
  endif
  return b:csv_max_col
endfunction

" Return regex to find the n-th column.
function! s:GetExpr(colnr, ...)
  if a:0 == 0  " field only
    let field = s:GetStr('expr1')
  else  " field with quotes (if present) and trailing delimiter (if present)
    let field = s:GetStr('expr2')
  endif
  if a:colnr > 1
    return s:GetStr('expr3') . (a:colnr - 1) . '}' . field
  else
    return '^' . field
  endif
endfunction

" Extract and echo the column header on the status line.
function! s:PrintColInfo(colnr)
  let colHeading = substitute(matchstr(getline(1), s:GetExpr(a:colnr)), '^\s*\(.*\)\s*$', '\1', '')
  let info = 'Column ' . a:colnr
  if empty(colHeading)
    echo info
  else
    echon info . ': '
    echohl Type
    " Limit length to avoid "Hit ENTER" prompt.
    echon strpart(colHeading, 0, (&columns / 2)) . (len(colHeading) > (&columns / 2) ? '...' : '')
    echohl NONE
  endif
endfunction

" Highlight n-th column (if n > 0).
" Remove previous highlight match (ignore error if none).
" matchadd() priority -1 means 'hlsearch' will override the match.
function! s:Highlight(colnr)
  silent! call matchdelete(b:csv_match)
  if a:colnr > 0
    if exists('*matchadd')
      let b:csv_match = matchadd('Keyword', s:GetExpr(a:colnr), -1)
    else
      execute '2match Keyword /' . s:GetExpr(a:colnr) . '/'
    endif
    if b:changed_done != b:changedtick
      let b:changed_done = b:changedtick
      call s:GetNumCols()
    endif
    call s:Focus_Col(a:colnr)
  endif
endfunction

" Focus the cursor on the n-th column of the current line.
function! s:Focus_Col(colnr)
  normal! 0
  call search(s:GetExpr(a:colnr), '', line('.'))
  call s:PrintColInfo(a:colnr)
endfunction

" Highlight next column.
function! s:HighlightNextCol()
  if b:csv_column < b:csv_max_col
    let b:csv_column += 1
  endif
  call s:Highlight(b:csv_column)
endfunction

" Highlight previous column.
function! s:HighlightPrevCol()
  if b:csv_column > 1
    let b:csv_column -= 1
  endif
  call s:Highlight(b:csv_column)
endfunction

" Wrapping would distort the column-based layout.
" Lines must not be broken when typed.
setlocal nowrap textwidth=0
" Undo the stuff we changed.
let b:undo_ftplugin = "setlocal wrap< textwidth<"
    \ . "|if exists('*matchdelete')|call matchdelete(b:csv_match)|else|2match none|endif"
    \ . "|sil! exe 'nunmap <buffer> H'"
    \ . "|sil! exe 'nunmap <buffer> L'"
    \ . "|sil! exe 'nunmap <buffer> J'"
    \ . "|sil! exe 'nunmap <buffer> K'"
    \ . "|sil! exe 'nunmap <buffer> <C-f>'"
    \ . "|sil! exe 'nunmap <buffer> <C-b>'"
    \ . "|sil! exe 'nunmap <buffer> 0'"
    \ . "|sil! exe 'nunmap <buffer> $'"
    \ . "|sil exe 'augroup csv' . bufnr('')"
    \ . "|sil exe 'au!'"
    \ . "|sil exe 'augroup END'"

let b:changed_done = -1
" Highlight the first column, but not if reloading or resetting filetype.
if !exists('b:csv_column')
  let b:csv_column = 1
endif
" Following highlights column and calls GetNumCols() if set filetype manually
" (BufEnter will also do it if filetype is set during load).
silent call s:Highlight(b:csv_column)

" Return Float value of field in line selected by regex, or the String field.
function! s:GetValue(line, regex)
  let field = matchstr(a:line, a:regex)
  let val = str2float(field)
  if val == 0 && match(field, '^0*\.\?0*$') < 0
    return field
  endif
  return val
endfunction

" Compare lines based on the floating point values in the specified column.
" This uses string compare 'ignorecase' option if neither field is a float.
function! s:CompareLines(line1, line2)
  let val1 = s:GetValue(a:line1, b:csv_sort_regex)
  let val2 = s:GetValue(a:line2, b:csv_sort_regex)
  if type(val1) != type(val2)
    let val1 = type(val1)
    let val2 = type(val2)
  endif
  let ascending = val1 > val2 ? 1 : val1 < val2 ? -1 : 0
  return b:csv_sort_ascending ? ascending : -ascending
endfunction

" Sort the n-th column, the highlighted one by default.
" Column number is first optional arg; following are flags for :sort.
function! s:SortCol(bang, ...) range
  let colnr = b:csv_column
  let args = copy(a:000)
  if len(args) > 0 && args[0] =~ '^\d\+$'
    let colnr = str2nr(args[0])
    unlet args[0]
  endif
  if colnr < 1 || colnr > b:csv_max_col
    call s:Warn('column number out of range.')
  endif
  let first = a:firstline == 1 ? 2 : a:firstline
  let flags = join(args)
  if flags == 'f'
    let b:csv_sort_ascending = (a:bang == '')
    let b:csv_sort_regex = s:GetExpr(colnr)
    call setline(first, sort(getline(first, a:lastline), function('s:CompareLines')))
  else
    let cmd = first.','.a:lastline.'sort'.a:bang
    execute cmd 'r'.flags '/'.escape(s:GetExpr(colnr), '/').'/'
  endif
endfunction
command! -bang -buffer -nargs=* -range=% Sort <line1>,<line2>call <SID>SortCol('<bang>', <f-args>)

" Delete the n-th column, the highlighted one by default.
function! s:DeleteCol(colnr)
  if a:colnr == ''
    let col = b:csv_column
  else
    let col = str2nr(a:colnr)
  endif
  if col < 1 || col > b:csv_max_col
    call s:Warn('column number out of range.')
  endif
  execute '%s/'.escape(s:GetExpr(col, 1), '/').'//'
  if col == b:csv_max_col
    execute 'silent %s/'.escape(s:GetStr('delco'), '/').'//e'
  endif
  let b:csv_max_col -= 1
  if b:csv_column > b:csv_max_col
    call s:HighlightPrevCol()
  endif
endfunction
command! -buffer -nargs=? DC call <SID>DeleteCol('<args>')

" Search the n-th column. Argument in n=regex form where n is the column
" number, and regex is the expression to use. If "n=" is omitted, then
" use the current highlighted column.
function! s:SearchCol(args)
  let [colstr, target] = matchlist(a:args, '\%(\([1-9][0-9]*\)=\)\?\(.*\)')[1:2]
  if colstr == ''
    let col = b:csv_column
  else
    let col = str2nr(colstr)
  endif
  if col < 1 || col > b:csv_max_col
    call s:Warn('column number out of range')
  endif
  if col == 1
    let @/ = s:GetStr('sear1').target.s:GetStr('sear2').target.s:GetStr('sear3')
  else
    let @/ = s:GetStr('sear4').(col-1).s:GetStr('sear5').target.s:GetStr('sear2').target.s:GetStr('sear3')
  endif
endfunction
" Use :SC n=string<CR> to search for string in the n-th column
command! -buffer -nargs=1 SC execute <SID>SearchCol('<args>')|normal! n
nnoremap <silent> <buffer> H :call <SID>HighlightPrevCol()<CR>
nnoremap <silent> <buffer> L :call <SID>HighlightNextCol()<CR>
nnoremap <silent> <buffer> J <Down>:call <SID>Focus_Col(b:csv_column)<CR>
nnoremap <silent> <buffer> K <Up>:call <SID>Focus_Col(b:csv_column)<CR>
nnoremap <silent> <buffer> <C-f> <PageDown>:call <SID>Focus_Col(b:csv_column)<CR>
nnoremap <silent> <buffer> <C-b> <PageUp>:call <SID>Focus_Col(b:csv_column)<CR>
nnoremap <silent> <buffer> <C-d> <C-d>:call <SID>Focus_Col(b:csv_column)<CR>
nnoremap <silent> <buffer> <C-u> <C-u>:call <SID>Focus_Col(b:csv_column)<CR>
nnoremap <silent> <buffer> 0 :let b:csv_column=1<CR>:call <SID>Highlight(b:csv_column)<CR>
nnoremap <silent> <buffer> $ :let b:csv_column=b:csv_max_col<CR>:call <SID>Highlight(b:csv_column)<CR>
nnoremap <silent> <buffer> gm :call <SID>Focus_Col(b:csv_column)<CR>
nnoremap <silent> <buffer> <LocalLeader>J J
nnoremap <silent> <buffer> <LocalLeader>K K

" The column highlighting is window-local, not buffer-local, so it can persist
" even when the filetype is undone or the buffer changed.
execute 'augroup csv' . bufnr('')
  autocmd!
  " These events only highlight in the current window.
  " Note: Highlighting gets slightly confused if the same buffer is present in
  " two split windows next to each other, because then the events aren't fired.
  autocmd BufLeave <buffer> silent call <SID>Highlight(0)
  autocmd BufEnter <buffer> silent call <SID>Highlight(b:csv_column)
augroup END
