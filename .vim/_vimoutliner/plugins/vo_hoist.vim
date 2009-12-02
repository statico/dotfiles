"######################################################################
"# VimOutliner Hoisting
"# Copyright (C) 2003 by Noel Henson noel@noels-lab.com
"# The file is currently an experimental part of Vim Outliner.
"#
"# This program is free software; you can redistribute it and/or modify
"# it under the terms of the GNU General Public License as published by
"# the Free Software Foundation; either version 2 of the License, or
"# (at your option) any later version.
"#
"# This program is distributed in the hope that it will be useful,
"# but WITHOUT ANY WARRANTY; without even the implied warranty of
"# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
"# GNU General Public License for more details.
"######################################################################

" Detailed Revision Log {{{1
"vo_hoist.vim
"Internal RCS
"$Revision: 1.10 $"
"$Date: 2005/06/12 15:53:54 $
"$Log: vo_hoist.vim,v $
"Revision 1.10  2005/06/12 15:53:54  noel
"Moved key mappings so they work with Matej' new way to load plugins.
"
"Revision 1.9  2003/11/12 17:26:09  noel
"Added a command to place the cursor on the first line of
"a hoisted outline.
"
"Revision 1.8  2003/11/12 17:10:51  noel
"Fixed a bug that occurs on a level 1 heading with no children.
"
"Revision 1.7  2003/10/23 22:14:14  noel
"Minor changes to DeHoist() to compensate for current foldlevel settings.
"
"Revision 1.6  2003/08/17 15:35:24  noel
"Put the new mappings in the correct place this time.
"Added a : and <cr> to the ZZ command.
"
"Revision 1.5  2003/08/17 14:47:42  noel
"Added ZZ, qa, and x to the list of commands that de-hoist the current
"outline.
"
"Revision 1.4  2003/08/17 00:07:31  noel
"Added "silent" to commands generating tedious messages.
"
"Revision 1.3  2003/08/16 20:08:06  noel
"Removed a need to exclude fold level 1 headings.
"
"Revision 1.2  2003/08/16 19:02:44  noel
"First fully functional version. May need some tweaks but it works and is
"quite easy to use.
"
"Revision 1.1  2003/08/14 21:05:05  noel
"First publicly available, experiment verison
"
"}}}2

" Load the plugin {{{1
" mappings {{{1
map <silent> <buffer> <localleader>h :call Hoist(line("."))<cr>
map <silent> <buffer> <localleader>H :call DeHoistThis(line("."))<cr>
"}}}1
if exists("g:did_vo_hoist")
	finish
endif
if !exists("g:hoistParanoia")
	let g:hoistParanoia=0
endif
let g:did_vo_hoist = 1
" Functions {{{1
" RemoveTabsLine(line,tabs) {{{2
" remove specified number of tabs from the begining of line
function! RemoveTabsLine(line,tabs)
	return substitute(getline(a:line),"^\\(\\t\\)\\{".a:tabs."}", "", "")
endfunction
"}}}2
" MakeTempFilename(line) {{{2
" return a string to use as the temporary filename for the hoisted area
function! MakeTempFilename(line)
	return "vo_hoist.".a:line.strftime(".%Y%m%d%H%M%S").".otl"
endfunction
"}}}2
" AddHoistFilename(line) {{{2
" Add a temporary filename to a parent line to indicate hoisting
function! AddHoistFilename(line)
	let l:newparent = getline(a:line)." __hoist:".MakeTempFilename(a:line)
	call setline(a:line,l:newparent)
endfunction
"}}}2
"}}}2
" DeleteHoistFilename(line) {{{2
" Delete a temporary filename from a parent line
function! DeleteHoistFilename(line)
	call setline(a:line,substitute(getline(a:line)," __hoist:.*","",""))
endfunction
"}}}2
" ExtractHoistFilename(line) {{{2
" Extract a filename from a hoisted parent
function! ExtractHoistFilename(line)
	return substitute(getline(a:line),".* __hoist:","","")
endfunction
"}}}2
" IsParent(line) {{{2
" Return 1 if this line is a parent
function! IsParent(line)
	return (Ind(a:line)+1) == Ind(a:line+1)
endfunction
"}}}2
" IsHoistedParent(line) {{{2
" Return 1 if this line is a parent with hoisted children
function! IsHoistParent(line)
	return match(getline(a:line)," __hoist:","") != -1 
endfunction
"}}}2
" FindParent(line) {{{2
" Return line if parent, parent line if not
function! FindParent(line)
	if IsParent(a:line)
		return a:line
	else
		let l:parentindent = Ind(a:line)-1
		let l:searchline = a:line
		while (Ind(l:searchline) != l:parentindent) && (l:searchline > 0)
			let l:searchline = l:searchline-1
		endwhile
		return l:searchline
	endif
endfunction
"}}}2
" FindLastChild(line) {{{2
" Return the line number of the last decendent of parent line
function! FindLastChild(line)
	let l:parentindent = Ind(a:line)
	let l:searchline = a:line+1
	while Ind(l:searchline) > l:parentindent
		let l:searchline = l:searchline+1
	endwhile
	return l:searchline-1
endfunction
"}}}2
"}}}2
" Hoist(line) {{{2
" Write the offspring of a parent to a new file, open it and remove the 
" leading tabs.
function! Hoist(line)
	let l:parent = FindParent(a:line)
	if l:parent == 0
		return
	endif
	call cursor(l:parent,1)
	let l:firstline = l:parent+1
	let l:childindent = Ind(l:firstline)
	let l:lastline = FindLastChild(l:parent)
	let l:filename = MakeTempFilename(l:parent)
	echo l:firstline.",".l:lastline."w! ".l:filename
	let l:folded = foldclosed(l:parent)
	call cursor(l:parent,1)
	normal zo
	exe l:firstline.",".l:lastline."w! ".l:filename
	call AddHoistFilename(l:parent)
	silent write
	" log what we did incase we need to recover manually
	let l:doit = l:parent."write! >> .vo_hoist.".bufname(bufnr("%")).".log"
	exe l:doit
	let l:parentbuffer = bufnr("%")
	"WARNING: switching files
	let l:doit = "silent e +%s/^\\\\(\\\t\\\\)\\\\{"
	let l:doit = l:doit.l:childindent."}// ".l:filename." | "
	let l:doit = l:doit."let b:myParentBuffer = ".l:parentbuffer." | "
	let l:doit = l:doit."let b:myParentLine = ".l:parent." | "
	let l:doit = l:doit."call cursor(1,1)|"
	let l:doit = l:doit."let b:hoisted = 1"
	exe l:doit
	silent write
endfunction
"}}}2
" DeleteChildren(line) {{{2
" Delete the existing offspring of a parent
function! DeleteChildren(line)
	let l:parent = FindParent(a:line)
	let l:firstline = l:parent+1
	let l:lastline = FindLastChild(l:parent)
	exe l:firstline.",".l:lastline."d"
endfunction
"}}}2
" MakeTabString(n) {{{2
" Return a string of n tabs
function! MakeTabString(n)
	let l:string = ""
	let l:i = 0
	while l:i < a:n
		let l:string = l:string."\t"
		let l:i = l:i +1
	endwhile
	return l:string
endfunction
"}}}2
" AddChildren(line) {{{2
" Add left-justified children to parent. The filename is extracted from the 
" end of the parent line. The parent is assumed to have no children at this 
" point.
function! AddChildren(line)
	let l:filename = ExtractHoistFilename(a:line)
	if filereadable(l:filename) == 1
		if a:line == line("$")
			exe "read ".l:filename
			if a:line != line("$")
				exe a:line+1.",$"." s/^/".MakeTabString(Ind(a:line)+1)."/"
			endif
		else
			exe a:line+1."ma v"
			call cursor(a:line,1)
			exe "read ".l:filename
			if a:line+1 != line("'v")
				exe a:line+1.",'v-1"." s/^/".MakeTabString(Ind(a:line)+1)."/"
			endif
		endif
	endif
endfunction
"}}}2
" DeleteHoistFile(line) {{{2
" Delete a temporary filename from a parent line
function! DeleteHoistFile(line)
	if g:hoistParanoia
		return
	endif
	let l:filename = ExtractHoistFilename(a:line)
	call delete(l:filename)
	let l:filename = l:filename."~"
	call delete(l:filename)
endfunction
"}}}2
" DeHoistThis(line) {{{2
" Remove the old children, add the new children and remove the __hoist data
" leading tabs from this file.
function! DeHoistThis(line)
	let l:parent = FindParent(a:line)
	let l:folded = foldclosed(l:parent)
	call cursor(l:parent,1)
	if l:folded == l:parent
		normal zo
	endif
	call DeleteChildren(l:parent)
	call AddChildren(l:parent)
	call DeleteHoistFile(l:parent)
	call DeleteHoistFilename(l:parent)
	if l:folded == l:parent
		normal zc
	endif
endfunction
"}}}2
" DeHoist() {{{2
" Remove the old children, add the new children and remove the __hoist data
" leading tabs from the calling file.
function! DeHoist()
	silent write
	if bufexists(b:myParentBuffer) == 0
		return
	endif
	let l:myParentBuffer = b:myParentBuffer
	let l:myParentLine = b:myParentLine
	bdelete
	" Warning switching files
	exe "buffer ".l:myParentBuffer
	call cursor(l:myParentLine,1)
	let l:parent = FindParent(l:myParentLine)
	let l:folded = foldclosed(l:parent)
	call cursor(l:parent,1)
" 	if l:folded == l:parent
" 		normal zo
" 	endif
 	normal zv
	silent call DeleteChildren(l:parent)
	silent call AddChildren(l:parent)
	silent call DeleteHoistFile(l:parent)
	silent call DeleteHoistFilename(l:parent)
	if l:folded == l:parent
		call cursor(l:parent,1)
		normal zc
	endif
	silent write
endfunction
"}}}2
"}}}1
" Autocommands {{{1
	au BufReadPost vo_hoist.*.otl cmap <buffer> wq call DeHoist()
	au BufReadPost vo_hoist.*.otl cmap <buffer> qa call DeHoist()
	au BufReadPost vo_hoist.*.otl cmap <buffer> q call DeHoist()
	au BufReadPost vo_hoist.*.otl cmap <buffer> x call DeHoist()
	au BufReadPost vo_hoist.*.otl nmap <buffer> ZZ :call DeHoist()<cr>
"}}}1
" vim600: set foldlevel=0 foldmethod=marker:
