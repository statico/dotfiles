" ReplaceWithRegister.vim: Replace text with the contents of a register. 
"
" DESCRIPTION:
"   Replacing an existing text with the contents of a register is a very common
"   task during editing. One typically first deletes the existing text via the
"   |d|, |D| or |dd| commands, then pastes the register with |p| or |P|. 
"   Most of the time, the unnamed register is involved, with the following
"   pitfall: If you forget to delete into the black-hole register ("_), the
"   replacement text is overwritten! 
"
"   This plugin offers a two-in-one command that replaces text covered by a
"   {motion}, entire line(s) or the current selection with the contents of a
"   register; the old text is deleted into the black-hole register. 
"
"   The replacement mode (characters or entire lines) is determined by the
"   replacement command / selection, not by the register contents. This avoids
"   surprises like when the replacement text was a linewise yank, but the
"   replacement is characterwise: In this case, no additional newline is
"   inserted. 
"
" USAGE:
"["x][count]gr{motion}	Replace {motion} text with the contents of register x. 
"			Especially when using the unnamed register, this is
"			quicker than "_d{motion}P or "_c{motion}<C-R>"
"["x][count]grr		Replace [count] lines with the contents of register x. 
"			To replace from the cursor position to the end of the
"			line use ["x]gr$ 
"{Visual}["x]gr		Replace the selection with the contents of register x. 
"
" INSTALLATION:
"   Put the script into your user or system Vim plugin directory (e.g.
"   ~/.vim/plugin). 

" DEPENDENCIES:
"   - Requires Vim 7.0 or higher. 
"   - repeat.vim (vimscript #2136) autoload script (optional). 
"
" CONFIGURATION:
"   The default mappings override the (rarely used, but somewhat related) |gr|
"   command (replace virtual characters under the cursor with {char}). 
"   If you want to use different mappings, map your keys to the
"   <Plug>ReplaceWithRegister... mapping targets _before_ sourcing this script
"   (e.g. in your |vimrc|): 
"	nmap <silent> <Leader>r <Plug>ReplaceWithRegisterOperator
"    	nmap <silent> <Leader>rr <Plug>ReplaceWithRegisterLine
"    	vmap <silent> <Leader>r <Plug>ReplaceWithRegisterOperator
"
" INTEGRATION:
" LIMITATIONS:
" ASSUMPTIONS:
" KNOWN PROBLEMS:
" TODO:
"
" Copyright: (C) 2008-2009 by Ingo Karkat
"   The VIM LICENSE applies to this script; see ':help copyright'. 
"
" Maintainer:	Ingo Karkat <ingo@karkat.de>
"
" REVISION	DATE		REMARKS 
"   1.02.009	25-Nov-2009	Replaced the <SID>Count workaround with
"				:map-expr and an intermediate
"				s:ReplaceWithRegisterOperatorExpression. 
"   1.01.008	06-Oct-2009	Do not define "gr" mapping for select mode;
"				printable characters should start insert mode. 
"   1.00.007	05-Jul-2009	Renamed from ingooperators.vim. 
"				Replaced g:register with call to
"				s:SetRegister(). 
"	006	05-Mar-2009	BF: Besides 'autoindent', 'indentexpr' also
"				causes additional indent. Now completely turning
"				off all these things via the 'paste' option. 
"	005	23-Feb-2009	BF: When replacing a complete line over an
"				indented line (with 'Vgr' or 'grr'), the old
"				indent was kept. Now temporarily turning off
"				'autoindent' to avoid that. 
"	004	20-Feb-2009	BF: ReplaceWithRegisterOperator mapping didn't
"				work for "last line" G motion, because v:count1
"				defaulted to line 1. Now checking v:count and
"				mapping to <Nop> if no count was given. 
"	003	01-Feb-2009	Allowing repeating via '.' by avoiding the
"				script error about undefined variable
"				g:register. 
"				Put try...finally around temporary 'selection'
"				setting. 
"				ENH: Now allowing [count] in front of
"				gr{motion} (see :help ingo-opfunc for details)
"				and grr (via repeat.vim). 
"				Now correcting mismatch when replacement is
"				linewise by temporarily removing the trailing
"				newline. 
"	002	15-Aug-2008	Added {Visual}gr and grr mappings. 
"	001	11-Aug-2008	file creation

" Avoid installing twice or when in unsupported Vim version. 
if exists('g:loaded_ReplaceWithRegister') || (v:version < 700)
    finish
endif
let g:loaded_ReplaceWithRegister = 1

function! s:SetRegister()
    let s:register = v:register
endfunction
function! s:ReplaceWithRegister( type )
    " It is important to delete into the black hole register, otherwise, the
    " unnamed register will be clobbered, and this may contain the text we want
    " to paste. 
    let l:replaceOnVisualSelectionCommand = '"_c' . "\<C-R>\<C-O>" . s:register . "\<Esc>"
    " If s:register is removed, repeat via '.' will fail on the above line,
    " because s:register is undefined. 
    "unlet s:register

    if a:type == 'visual'
	" Note: The 'cc' command (and thus also 'c' on a linewise visual
	" selection) preserves the indent of the first line if 'autoindent' is
	" on (cp. :help cc), or if there is an 'indentexpr'. As we want a
	" replacement exactly inside the former visual selection, we temporarily
	" turn off all these things by setting 'paste'. 
	setlocal paste
	try
	    execute 'normal! gv' . l:replaceOnVisualSelectionCommand
	finally
	    setlocal nopaste
	endtry
    else
	" Note: :normal! `[c`] would keep the last moved-over character, as 'c'
	" changes until the mark, but does not include it. The easiest way to get
	" around this is to first establish a visual selection, then change that.
	" However, even here we need to make sure to be "inclusive"! 
	let l:save_selection = &selection
	set selection=inclusive
	try
	    execute 'normal! `[v`]' . l:replaceOnVisualSelectionCommand
	finally
	    let &selection = l:save_selection
	endtry
    endif
endfunction
function! s:ReplaceWithRegisterOperator( type )
    let l:pasteText = getreg(s:register)
    let l:regType = getregtype(s:register)
    if l:regType ==# 'V' && l:pasteText =~# '\n$'
	" Our custom operator is linewise, even in the ReplaceWithRegisterLine
	" variant, in order to be able to replace less than entire lines (i.e.
	" characterwise yanks). 
	" So there's a mismatch when the replacement text is a linewise yank,
	" and the replacement would put an additional newline to the end.
	" To fix that, we temporarily remove the trailing newline character from
	" the register contents and set the register type to characterwise yank. 
	call setreg(s:register, strpart(l:pasteText, 0, len(l:pasteText) - 1), 'v')
    endif
    try
	call s:ReplaceWithRegister(a:type)
    finally
	if l:regType ==# 'V' && l:pasteText =~# '\n$'
	    " Undo the temporary change of the register. 
	    call setreg(s:register, l:pasteText, l:regType)
	endif
    endtry
endfunction
function! s:ReplaceWithRegisterOperatorExpression( opfunc )
    call s:SetRegister()
    let &opfunc = a:opfunc
    return 'g@'
endfunction

" This mapping repeats naturally, because it just sets a global things, and Vim
" is able to repeat the g@ on its own. 
nnoremap <expr> <Plug>ReplaceWithRegisterOperator <SID>ReplaceWithRegisterOperatorExpression('<SID>ReplaceWithRegisterOperator')
" This mapping needs repeat.vim to be repeatable, because it contains of
" multiple steps (visual selection + 'c' command inside
" s:ReplaceWithRegisterOperator). 
nnoremap <silent> <Plug>ReplaceWithRegisterLine     :<C-u>call <SID>SetRegister()<Bar>execute 'normal! V' . v:count1 . "_\<lt>Esc>"<Bar>call <SID>ReplaceWithRegisterOperator('visual')<bar>silent! call repeat#set("\<lt>Plug>ReplaceWithRegisterLine")<CR>
" Repeat not defined in visual mode. 
vnoremap <Plug>ReplaceWithRegisterOperator :<C-u>call <SID>SetRegister()<Bar>call <SID>ReplaceWithRegisterOperator('visual')<CR>

if ! hasmapto('<Plug>ReplaceWithRegisterOperator', 'n')
    nmap <silent> gr <Plug>ReplaceWithRegisterOperator
endif
if ! hasmapto('<Plug>ReplaceWithRegisterLine', 'n')
    nmap <silent> grr <Plug>ReplaceWithRegisterLine
endif
if ! hasmapto('<Plug>ReplaceWithRegisterOperator', 'x')
    xmap <silent> gr <Plug>ReplaceWithRegisterOperator
endif

" vim: set sts=4 sw=4 noexpandtab ff=unix fdm=syntax :
