syntax clear 

syn match styleFileWord     "{[^}]\+}"
syn match styleFileComment  "^#\s.*$"

hi! def link styleFileWord      Identifier
hi! def link styleFileComment   Comment

let b:current_syntax = "stylefile"

nmap <buffer> <leader>w i{<Esc>ea}<Esc>b

call setreg('/','{[^}]\+}')
nohlsearch

set nowrap
set tw=0

