" Tab-separated syntax file.
" Based off yntax file for NoSQL table. v0.1
" Based on VimOutliner syntax file.
"
" Licensed under GPLv2
" Comes with ABSOLUTELY NO WARRANTY: See nosql.txt for details.
"

syntax clear
syntax region Field01 matchgroup=Field01 start=+^\p+ end=+$+ contains=Field02
syntax region Field02 matchgroup=Field02 start=+\t+ end=+$+ contains=Field03 contained keepend
syntax region Field03 matchgroup=Field03 start=+\t+ end=+$+ contains=Field04 contained keepend
syntax region Field04 matchgroup=Field04 start=+\t+ end=+$+ contains=Field05 contained keepend
syntax region Field05 matchgroup=Field05 start=+\t+ end=+$+ contains=Field06 contained keepend
syntax region Field06 matchgroup=Field06 start=+\t+ end=+$+ contains=Field07 contained keepend
syntax region Field07 matchgroup=Field07 start=+\t+ end=+$+ contains=Field08 contained keepend
syntax region Field08 matchgroup=Field08 start=+\t+ end=+$+ contains=Field09 contained keepend
syntax region Field09 matchgroup=Field09 start=+\t+ end=+$+ contains=Field10 contained keepend
syntax region Field10 matchgroup=Field10 start=+\t+ end=+$+ contains=Field11 contained keepend
syntax region Field11 matchgroup=Field11 start=+\t+ end=+$+ contains=Field12 contained keepend
syntax region Field12 matchgroup=Field12 start=+\t+ end=+$+ contains=Field13 contained keepend
syntax region Field13 matchgroup=Field13 start=+\t+ end=+$+ contains=Field14 contained keepend
syntax region Field14 matchgroup=Field14 start=+\t+ end=+$+ contains=Field15 contained keepend
syntax region Field15 matchgroup=Field15 start=+\t+ end=+$+ contains=Field16 contained keepend
syntax region Field16 matchgroup=Field16 start=+\t+ end=+$+ contained keepend

hi Field01 ctermfg=black term=bold

hi Field02 ctermfg=cyan
hi Field03 ctermfg=green
hi Field04 ctermfg=red
hi Field05 ctermfg=yellow
hi Field06 ctermfg=blue
hi Field07 ctermfg=magenta

hi Field08 ctermfg=cyan cterm=bold
hi Field09 ctermfg=green cterm=bold
hi Field10 ctermfg=red cterm=bold
hi Field11 ctermfg=yellow cterm=bold
hi Field12 ctermfg=blue cterm=bold
hi Field13 ctermfg=magenta cterm=bold

hi Field14 ctermfg=cyan
hi Field15 ctermfg=green
hi Field16 ctermfg=red

set noexpandtab
set nosmarttab
set tabstop=10
set shiftwidth=10
set nolist
