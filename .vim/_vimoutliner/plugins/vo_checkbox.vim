"######################################################################
"# VimOutliner Checkboxes
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
"vo_checkbox.vim
"Internal RCS
"$Revision: 1.22 $"
"$Date: 2005/06/11 16:34:25 $
"$Log: vo_checkbox.vim,v $
"Revision 1.22  2005/06/11 16:34:25  noel
"Modified ,,cz to recompute completion on an entire tree.
"
"Revision 1.21  2005/06/11 15:54:05  noel
"Modified the behavior of ,,cp to always include the % sign. Even on childless
"parents.
"
"Revision 1.20  2005/06/11 14:01:42  noel
"Modified computation of percentage completion to work on a heading
"that has not children. Now if a childless heading is marked done,
"the percentage shown will be 100% instead of 0%.
"
"Revision 1.18  2005/06/07 15:08:59  noel
"Fixed a bug that added checkboxes to body text, preformatted body text,
"tables, user-defined text and user-defined preformatted text lines.
"Fixed a bug where ,,cb would modify terms like 'test%' or ',,c%'. Now
"it only modifies this term: ' [0-9]*%'.
"
"Revision 1.17  2005/06/07 13:53:22  noel
"Added ,,cp. It is the same as ,,c%
"
"Revision 1.16  2004/05/27 22:22:48  noel
"Made ,,cd smart so it would try to delete non-existent checkboxes.
"Fixed a recursion bug in NewHMD to branches with a single child would
"be computed properly.
"
"Revision 1.15  2004/05/27 18:11:53  noel
"Added smart (only the entire tree in which the child exists), automatic
"completion calculations to the ,,cx command.
"Added smart (only for parents, not children) '%' sign insertion for
",,c%.
"
"Revision 1.14  2004/05/17 15:53:38  noel
"Modified SwitchBox() to be more selective.
"
"Revision 1.13  2004/05/17 15:43:23  noel
"Fixed a broken key mapping: ,,c%.
"
"Revision 1.12  2004/02/23 12:19:27  noel
"Fixed Up-to-date problem.
"Fixed 'Safely script names'.
"
"Revision 1.11  2003/09/05 16:37:55  cepl
"Added ,cp binding for the new function InsertCheckBoxPerCent,
"which adds not only the checkbox but also percentage sign.
"
"Revision 1.10  2003/08/23 16:42:15  noel
"Modified completion percentages to be recursive.
"This:
"[_] 0% Project
"	[_] 33% Task
"		[_] Subtask
"		[X] Subtask
"		[X] Subtask
"	[_] 0% Task
"		[_] Subtask
"		[_] 50% Subtask
"			[X] Subsubtask
"			[_] Subsubtask
"			[X] Subsubtask
"			[_]  Subsubtask
"
"Becomes this:
"[_] 29% Project
"	[_] 33% Task
"		[_] Subtask
"		[X] Subtask
"		[X] Subtask
"	[_] 25% Task
"		[_] Subtask
"		[_] 50% Subtask
"			[X] Subsubtask
"			[_] Subsubtask
"			[X] Subsubtask
"			[_]  Subsubtask
"
"Revision 1.9  2003/08/16 13:49:53  noel
"added ! to functions.
"
"Revision 1.8  2003/08/11 19:16:28  noel
"Fixed a bug in which any line that contained the letter x was counted as
"completed.
"
"Revision 1.7  2003/08/03 23:56:46  noel
"
"Replaced HowManyDone() with a new routine. The new routine is recursive,
"ignores headings that don't have checkboxes and does not care what the
"current folding states of the parent or children are. The heading at the
"top of the tree does not even need to have a checkbox. This will work:
"
"Projects
"	[_] Software
"		[_] Input
"		[_] Processing
"			[_] Math
"			[_] Database
"			[_] Networking
"		[_] Output
"	[_] Hardware
"		[_] Keyboard
"		[_] Harddisk
"		[_] Processor
"		[_] Printer
"
"One only needs to ,,cx on Projects to update everything (everything shown).
"As before, including a % on a parent heading with childred, will be replaced
"with a percentage of completion. Nice work Matej!
"
"Revision 1.6  2003/07/14 00:36:57  noel
"Changed [x] to [X] to make it look more full. If the consensus is
"[x], I'll gladly put it back.
"
"Revision 1.5  2003/07/10 16:29:50  cepl
"Calculation of the subtree completion added. The very first alpha
"draft.
"
"Revision 1.3  2003/07/09 13:10:57  matej
"The first draft of the work done in subtree.
"
"Revision 1.2  2003/07/08 17:58:56  matej
"Pøidáno ,cx a ,ci zmìnìn na ,cb.
"
"Revision 1.1  2003/07/08 17:39:12  matej
"Initial revision
"
"Revision 1.3  2003/07/07 14:17:04  noel
"Fixed the folding of the new headers.
"
"Revision 1.2  2003/07/07 14:14:02  noel
"Added appropriate headers.
"}}}1

" InsertCheckBox() {{{1
" Insert a checkbox at the beginning of a header without disturbing the
" current folding.
function! InsertCheckBox()
	let @x = "[_] "
	normal! ^"xP
endfunction
"}}}1
" Safely InsertCheckBox() {{{1
" Insert a checkbox at the beginning of a header without disturbing the
" current folding only if there is no checkbox already.
function! SafelyInsertCheckBox()
	if match(getline("."),"^\t\t*\[<>:;|\]") != -1
		return
	endif
	if match(getline("."),"[\[X_\]]") == -1
		let @x = "[_] "
		normal! ^"xP
	endif
endfunction
"}}}1
" Safely InsertCheckBoxPercent() {{{1
" Insert a checkbox and % sign at the beginning of a header without disturbing 
" the current folding only if there is no checkbox already.
function! SafelyInsertCheckBoxPercent()
	if match(getline("."),"^\t\t*\[<>:;|\]") != -1
		return
	endif
        if match(getline("."), "[\[X_\]]") == -1
		if Ind(line(".")+1) > Ind(line("."))
			let @x = "[_] % "
		else
			let @x = "[_] "
		endif
           normal! ^"xP
        endif
endfunction
"}}}1
" Safely InsertCheckBoxPercentAlways() {{{1
" Insert a checkbox and % sign at the beginning of a header without disturbing 
" the current folding only if there is no checkbox already. Include the 
" checkbox even on childless headings.
function! SafelyInsertCheckBoxPercentAlways()
	if match(getline("."),"^\t\t*\[<>:;|\]") != -1
		return
	endif
        if match(getline("."), "[\[X_\]]") == -1
		let @x = "[_] % "
           normal! ^"xP
        endif
endfunction
"}}}1
" SwitchBox() {{{1
" Switch the state of the checkbox on the current line.
function! SwitchBox()
   let questa = strridx(getline("."),"[_]")
   let questb = strridx(getline("."),"[X]")
   if (questa != -1) || (questb != -1)
	   if (questa != -1) 
	      substitute/\[_\]/\[X\]/
	   else
	      substitute/\[X\]/\[_\]/
	   endif
   endif
endfunction
"}}}1
" DeleteCheckbox() {{{1
" Delete a checkbox if one exists
function! DeleteCheckbox()
   let questa = strridx(getline("."),"[_]")
   let questb = strridx(getline("."),"[X]")
   if (questa != -1) || (questb != -1)
	   if (questa != -1) 
	      substitute/\(^\s*\)\[_\] \(.*\)/\1\2/
	   else
	      substitute/\(^\s*\)\[X\] \(.*\)/\1\2/
	   endif
   endif
endfunction
"}}}1
" Ind(line) {{{1
" Return the index of the line.
" Remove it when using the new version of VO
function! Ind(line)
	return indent(a:line) / &tabstop
endf
" FindRootParent(line) {{{1
" returns the line number of the root parent for any child
function! FindRootParent(line)
	if Ind(a:line) == 0
		return (a:line)
	endif
	let l:i = a:line
	while l:i > 1 && Ind(l:i) > 0
		let l:i = l:i - 1
	endwhile
	return l:i
endf

" NewHMD(line) {{{1
" (How Many Done) 
" Calculates proportion of already done work in the subtree
function! NewHMD(line)
	let l:done = 0
	let l:count = 0
	let l:i = 1
	while Ind(a:line) < Ind(a:line+l:i)
		if (Ind(a:line)+1) == (Ind(a:line+l:i))
			let l:childdoneness = NewHMD(a:line+l:i)
			if l:childdoneness >= 0
				let l:done = l:done + l:childdoneness
				let l:count = l:count+1
			endif
		endif
		let l:i = l:i+1
	endwhile
   let l:proportion=0
   if l:count>0
     let l:proportion = ((l:done * 100)/l:count)/100
   else
      if match(getline(a:line),"\\[X\\]") != -1
	      let l:proportion = 100
      else 
	      let l:proportion = 0
      endif
   endif
   call setline(a:line,substitute(getline(a:line)," [0-9]*%"," ".l:proportion."%",""))
   if l:proportion == 100
      call setline(a:line,substitute(getline(a:line),"\\[.\\]","[X]",""))
      return 100
   elseif l:proportion == 0 && l:count == 0
      if match(getline(a:line),"\\[X\\]") != -1
	      return 100
      elseif match(getline(a:line),"\\[_\\]") != -1
	      return 0
      else
	      return -1
      endif
   else
      call setline(a:line,substitute(getline(a:line),"\\[.\\]","[_]",""))
      return l:proportion
   endif
endf

" mappings {{{1
" insert a chechbox
map <buffer> <localleader>cb :call SafelyInsertCheckBox()<cr>
map <buffer> <localleader>c% :call SafelyInsertCheckBoxPercent()<cr>
map <buffer> <localleader>cp :call SafelyInsertCheckBoxPercentAlways()<cr>
map <buffer> <localleader>cB :call InsertCheckBox()<cr>

" delete a chechbox
map <buffer> <localleader>cd :call DeleteCheckbox()<cr>

" switch the status of the box
map <buffer> <localleader>cx :call SwitchBox()<cr>:call NewHMD(FindRootParent(line(".")))<cr>

" calculate the proportion of work done on the subtree
map <buffer> <localleader>cz :call NewHMD(FindRootParent(line(".")))<cr>

"}}}1

" vim600: set foldlevel=0 foldmethod=marker:
