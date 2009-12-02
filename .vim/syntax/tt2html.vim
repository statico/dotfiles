" Vim syntax file

" Language: TT2 ( Inner HTML )

" Last Change:  21 Nov 2003

" Maintainar:   Atsushi Moriki <four@olive.freemail.ne.jp>



runtime! syntax/html.vim

unlet b:current_syntax



so <sfile>:p:h/tt2.vim

unlet b:current_syntax

syn cluster htmlPreProc add=@tt2_top_cluster



