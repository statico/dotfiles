" FILE:     syntax/conque.vim
" AUTHOR:   Nico Raffo <nicoraffo@gmail.com>
"           Shougo Matsushita <Shougo.Matsu@gmail.com> (original VimShell)
"           Yukihiro Nakadaira (vimproc)
" MODIFIED: 2009-12-17
" VERSION:  0.6, for Vim 7.0
" LICENSE: {{{
" Conque - pty interaction in Vim
" Copyright (C) 2009 Nico Raffo 
"
" MIT License
" 
" Permission is hereby granted, free of charge, to any person obtaining a copy
" of this software and associated documentation files (the "Software"), to deal
" in the Software without restriction, including without limitation the rights
" to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
" copies of the Software, and to permit persons to whom the Software is
" furnished to do so, subject to the following conditions:
" 
" The above copyright notice and this permission notice shall be included in
" all copies or substantial portions of the Software.
" 
" THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
" IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
" FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
" AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
" LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
" OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
" THE SOFTWARE.
" }}}

" MySQL
syn match MySQLTableHead "^|.*| \?$" nextgroup=MySQLTableDivide contains=MySQLTableBar oneline skipwhite skipnl
syn match MySQLTableBody "^|.*| \?$" nextgroup=MySQLTableBody,MySQLTableEnd contains=MySQLTableBar,MySQLNull,MySQLBool,MySQLNumber,MySQLStorageClass oneline skipwhite skipnl
syn match MySQLTableEnd "^+[+-]\++$" oneline 
syn match MySQLTableDivide "^+[+-]\++$" nextgroup=MySQLTableBody oneline skipwhite skipnl
syn match MySQLTableStart "^+[+-]\++$" nextgroup=MySQLTableHead oneline skipwhite skipnl
syn match MySQLTableBar "|" contained
syn match MySQLNull " NULL " contained
syn match MySQLBool " YES " contained
syn match MySQLBool " NO " contained
syn match MySQLStorageClass " PRI " contained
syn match MySQLStorageClass " MUL " contained
syn match MySQLStorageClass " UNI " contained
syn match MySQLStorageClass " CURRENT_TIMESTAMP " contained
syn match MySQLStorageClass " auto_increment " contained
syn match MySQLNumber " \d\+ " contained
syn match MySQLQueryStat "^\d\+ rows\? in set.*" oneline
syn match MySQLPrompt "^.\?mysql>" oneline
syn match MySQLPrompt "^    ->" oneline

syn case ignore
syn keyword Keyword select count max show table status like as from left right outer inner join where group by having limit offset order desc asc show
syn case match

" Typical Prompt
"syn match ConquePrompt '^.*\$' oneline
syn region ConqueString start=+'+ end=+'+ skip=+\\'+ contains=all oneline
syn region ConqueString start=+"+ end=+"+ skip=+\\"+ contains=all oneline
syn region ConqueString start=+`+ end=+`+ skip=+\\`+ contains=all oneline
hi def link ConqueString String

hi def link MySQLPrompt Identifier
hi def link MySQLTableHead Title
hi def link MySQLTableBody Normal
hi def link MySQLTableEnd Comment
hi def link MySQLTableDivide Comment
hi def link MySQLTableStart Comment
hi def link MySQLTableBar Comment
hi def link MySQLNull Comment
hi def link MySQLBool Boolean
hi def link MySQLStorageClass Type
hi def link MySQLNumber Number
hi def link MySQLQueryStat Comment

" vim: foldmethod=marker
