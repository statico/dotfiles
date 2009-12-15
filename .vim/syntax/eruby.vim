" Vim syntax file
" Language:	   eruby
" Maintainer:  Michael Brailsford <brailsmt@yahoo.com>
" Installation:
"	To automatilcally load this file when a .rhtml file is opened, add the
"	following lines to ~/.vim/filetype.vim:
"
"		augroup filetypedetect
" 			au! BufRead,BufNewFile *.rhtml		setfiletype eruby
" 		augroup END
"
"	You will have to restart vim for this to take effect.  In any case it 
"	is a good idea to read ":he new-filetype" so that you know what is going
"	on, and why the above lines work.

if version < 600
	syntax clear
elseif exists("b:current_syntax")
	finish
endif

"Source the html syntax file
ru! syntax/html.vim
"Set the filetype to html to load the html ftplugins
set ft=html
unlet b:current_syntax

"Put the ruby syntax file in @rubyTop
syn include @rubyTop syntax/ruby.vim

syn region erubyBlock matchgroup=erubyRubyDelim start=#<%=\?# end=#%># keepend containedin=ALL contains=@rubyTop,erubyEnd
syn region erubyComment start=+<%#+ end=#%># keepend
syn match erubyEnd #\<end\>#

hi link erubyRubyDelim todo
hi link erubyComment comment
hi link erubyEnd rubyControl

" vim: set ts=4 sw=4:
