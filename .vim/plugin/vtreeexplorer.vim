"" File:        vtreeexplorer.vim
"" Description: tree-like file system explorer for vim
"" Version:     $Revision: 1.20 $ $Date: 2004/12/30 18:28:31 $
"" Author:      TS Urban (tsurban@HORMELcomcast.net)
""              (remove HORMEL from my email first)
""
"" Instructions:
""   1 - source this file or put in your plugin directory
""   2 - :VTreeExlorer or :VSTreeExplore
""   3 - help at top of screen
""   4 - this script comes with a help text that integrates with the vim help
""       system, put vtreeexplorer.txt in your ~/.vim/doc dir, then do
""         :helptags ~/.vim/doc
""
"" Global Configuration Variables:
""  treeExplVertical : split vertically when starting with VSTreeExplore
""  treeExplWinSize  : window size (width or height) when doing VSTreeExplore
""  treeExplHidden   : set to have explorer start with hidden files shown
""  treeExplDirSort  : start explorer with desired directory sorting:
""    0 : no directory sorting
""    1 : directories sorting first
""   -1 : directories sorting last
""
"" Todo:
""   - global option for path separator
""   - merge in patches for winmanager
""   - +/- keymappings, etc
""   - recursively collapse binding/function

"" prevent multiple loading unless developing with g:treeExplDebug
if exists("vloaded_tree_explorer") && !exists("g:treeExplDebug")
	finish
endif
let vloaded_tree_explorer=1

let s:cpo_save = &cpo
set cpo&vim

"" create commands
command! -n=? -complete=dir VTreeExplore :call s:TreeExplorer(0, '<a>')
command! -n=? -complete=dir VSTreeExplore :call s:TreeExplorer(1, '<a>')

"" support sessions
autocmd BufNewFile TreeExplorer VTreeExplore

"" TreeExplorer() - set up explorer window
function! s:TreeExplorer(split, start) " <<<

	" dir to start in from arg, buff dir, or pwd
	let fname = (a:start != "") ? a:start : expand ("%:p:h")
	let fname = (fname != "") ? fname : getcwd ()

	" construct command to open window
	if a:split || &modified
		" if starting with split, get split parameters from globals
		let splitMode = (exists("g:treeExplVertical")) ? "vertical " : ""
		let splitSize = (exists("g:treeExplWinSize")) ? g:treeExplWinSize : 20
		let cmd = splitMode . splitSize . "new TreeExplorer"
	else
		let cmd = "e TreeExplorer"
	endif
	silent execute cmd

	"" chars to escape in file/dir names - TODO '+' ?
	let w:escape_chars =  " `|\"~'#"

	" win specific vars from globals if they exist
	let w:hidden_files = (exists("g:treeExplHidden")) ? 1 : 0
	let w:dirsort = (exists("g:treeExplDirSort")) ? g:treeExplDirSort : 0
	if w:dirsort < -1 || w:dirsort > 1
		let w:dirsort = 0
		" TODO - neede?
		let w:escape_chars = w:escape_chars . '+'
	endif

	" init help to short version
	let w:helplines = 1

	" throwaway buffer options
	setlocal noswapfile
	setlocal buftype=nowrite
	setlocal bufhidden=delete " d
	setlocal nowrap
	iabc <buffer>

	" setup folding for markers that will be inserted
	setlocal foldmethod=marker
	setlocal foldtext=substitute(getline(v:foldstart),'.{{{.*','','')
	setlocal foldlevel=1

  " syntax highlighting
  if has("syntax") && exists("g:syntax_on") && !has("syntax_items")
    syn match treeHlp  #^" .*#
    syn match treeDir  "^\.\. (up a directory)$"
		syn match treeFld  "{{{"
		syn match treeFld  "}}}"
		syn match treePrt  #|   #
		syn match treePrt  #|-- #
		syn match treePrt  #`-- #
		"syn match treeLnk  #[^-| `].* -> # contains=treeFld,treePrt
		syn match treeLnk  #[^-| `].* -> # contains=treeFld
    "syn match treeDir  #.*/\([ {}]\{4\}\)*$# contains=treePrt,treeFld,treeLnk
    syn match treeDir  #[^-| `].*/\([ {}]\{4\}\)*$# contains=treeFld,treeLnk
    syn match treeCWD  #^/.*$# contains=treeFld

		hi def link treePrt Normal
		hi def link treeFld Ignore
    hi def link treeHlp Special
    hi def link treeDir Directory
    hi def link treeCWD Statement
		hi def link treeLnk Title
  endif

	" for line continuation
  let cpo_save1 = &cpo
  set cpo&vim

	" set up mappings and commands for this buffer
  nnoremap <buffer> <cr> :call <SID>Activate()<cr>
  nnoremap <buffer> o    :call <SID>Activate()<cr>
	nnoremap <buffer> X    :call <SID>RecursiveExpand()<cr>
	nnoremap <buffer> E    :call <SID>OpenExplorer()<cr>
  nnoremap <buffer> C    :call <SID>ChangeTop()<cr>
  nnoremap <buffer> H    :call <SID>InitWithDir($HOME)<cr>
	nnoremap <buffer> u    :call <SID>ChdirUp()<cr>
	nnoremap <buffer> p    :call <SID>MoveParent()<cr>
	nnoremap <buffer> r    :call <SID>RefreshDir()<cr>
  nnoremap <buffer> R    :call <SID>InitWithDir("")<cr>
	nnoremap <buffer> S    :call <SID>StartShell()<cr>
	nnoremap <buffer> D    :call <SID>ToggleDirSort()<cr>
	nnoremap <buffer> a    :call <SID>ToggleHiddenFiles()<cr>
  nnoremap <buffer> ?    :call <SID>ToggleHelp()<cr>
	nnoremap <buffer> <2-leftmouse> :call <SID>Activate()<cr>

	command! -buffer -complete=dir -nargs=1 CD :call s:TreeCD('<a>')
	command! -buffer -range -nargs=0 Yank :<line1>,<line2>y |
				\ let @" = substitute (@", ' [{}]\{3\}', "", "g")

  let &cpo = cpo_save1 " restore

	call s:InitWithDir(fname) " load fname dir
endfunction " >>>

"" TreeCD() - change to dir from cmdline arg
function! s:TreeCD(dir) " <<<
	if isdirectory (a:dir)
		call s:InitWithDir (a:dir)
	else
		echo "can not change to directory: " . a:dir
	endif
endfunction " >>>

"" InitWithDir() - reload tree with dir
function! s:InitWithDir(dir) " <<<
	if a:dir != ""
		execute "lcd " . escape (a:dir, w:escape_chars)
	endif
	let cwd = getcwd ()

	" clear buffer
	setlocal modifiable | silent! normal ggdG
	setlocal nomodifiable

	"insert header
	call s:AddHeader()
	normal G

	let save_f=@f

	"insert parent link unless we're at / for unix or X:\ for dos
	if cwd != "/" && cwd !~ '^[^/]..$'
		let @f=".. (up a directory)"
	endif
	let @f=@f . "\n" . cwd  . "/\n\n"

	setlocal modifiable | silent put f | setlocal nomodifiable

	normal Gk

	call s:ReadDir (line("."), cwd) " read dir

	let @f=save_f
endfunction " >>>

"" ReadDir() -  read dir after current line with tree pieces and foldmarkers
function! s:ReadDir(lpn,dir) " <<<
	let olddir = getcwd ()

	let lps = getline (a:lpn)

	if a:dir == ""
		let dir = GetAbsPath2 (lpn, 0)
		if w:firstdirline ! = lpn
			echo "ERROR"
			return
		endif
	else
		let dir = a:dir
	endif

	" TODO - error when dir no longer exists
	execute "lcd " . escape (dir, w:escape_chars)

	" @l is used for first line, last line, and if dir sorting is off
	let save_l = @l | let @l = ""

	" @f and @d are used for file and dirs with dir sorting
	if w:dirsort != 0
		let save_d = @d | let @d = ""
		let save_f = @f | let @f = ""
	endif

	" change dos path to look like unix path
	if has("unix") == 0 " TODO ?
		let dir = substitute (dir, '\\', '/', "g")
	endif
	let dir = substitute (dir, '/\?$', '/', "")
	
	" get dir contents
	if w:hidden_files == 1
		"let dirlines = glob ('.*') . "\n" . glob ('*')
    let dirlines = system("find . -maxdepth 1 -mindepth 1 -not -name 'CVS' -and -not -name '*.class' -printf '%P\n' | sort")
	else
		"let dirlines = glob ('*')
    let dirlines = system("find . -maxdepth 1 -mindepth 1 -not -name 'CVS' -and -not -name '*.class' -and -not -name '.*' -printf '%P\n' | sort")
	endif

	" if empty, don't change line
	if dirlines == ""
		return
	endif

	let treeprt = substitute (lps, '[^-| `].*', "", "")
	let pdirprt = substitute (lps, '^[-| `]*', "", "")
	let pdirprt = substitute (pdirprt, '[{} ]*$', "", "")
	let foldprt = substitute (lps, '.*' . pdirprt, "", "")

	let @l = treeprt . pdirprt . ' {{{'

	let treeprt = substitute (treeprt, '`-- ', '    ', "")
	let treeprt = substitute (treeprt, '|-- ', '|   ', "")

	" parse dir contents by '/'
	let dirlines = substitute (dirlines, "\n", '/', "g")

	while strlen (dirlines) > 0
		let curdir = substitute (dirlines, '/.*', "", "")
		let dirlines = substitute (dirlines, '[^/]*/\?', "", "")

		if w:hidden_files == 1 && curdir =~ '^\.\.\?$'
			continue
		endif

		let linkedto = resolve (curdir)
		if linkedto != curdir
			let curdir = curdir . ' -> ' . linkedto
		endif
		if isdirectory (linkedto)
			let isdir = 1
			let curdir = curdir . '/'
		else
			let isdir = 0
		endif

		" escape leading characters confused with tree parts
		if curdir =~ '^[-| `]'
			let curdir = '\' . curdir
		endif

		if w:dirsort != 0
			if isdir == 1
				let @d = @d . "\n" . treeprt . '|-- ' . curdir
			else
				let @f = @f . "\n" . treeprt . '|-- ' . curdir
			endif
		else
			let @l = @l . "\n" . treeprt . '|-- ' . curdir
		endif
	endwhile

	if w:dirsort == 1
		let @l = @l .  @d . @f . "\n"
	elseif w:dirsort == -1
		let @l = @l .  @f . @d . "\n"
	else
		let @l = @l . "\n"
	endif

	exec (":" . a:lpn)

	" TODO handle fold open v fold closed
	setlocal modifiable
	silent normal ddk
	silent put l
	setlocal nomodifiable

	" make sure fold is open so we don't delete the whole thing
	"if foldclosed (line (".")) != -1
	if foldclosed (a:lpn) != -1
		foldopen
	endif

	normal! `]

	" change last tree part to the final leaf marking, add final fold mark
	let @l = getline(".")
	let @l = substitute (@l, '|-- ', '`-- ', "")
	let @l = @l . foldprt . " }}}\n"

	setlocal modifiable | silent normal dd
	silent put! l | setlocal nomodifiable

	" restore registers
	let @l = save_l
	if w:dirsort != 0
		let @d = save_d
		let @f = save_f
	endif

	exec (":" . a:lpn)

	execute "lcd " . escape (olddir, w:escape_chars)
endfunction " >>>

"" ChdirUp() -  cd up (if possible)
function! s:ChdirUp() " <<<
	let cwd = getcwd()
	if cwd == "/" || cwd =~ '^[^/]..$'
		echo "already at top dir"
	else
		call s:InitWithDir("..")
	endif
endfunction " >>>

"" MoveParent() - move cursor to parent dir
function! s:MoveParent() " <<<
	let ln = line(".")
	call s:GetAbsPath2 (ln, 1)
	if w:firstdirline != 0
		exec (":" . w:firstdirline)
	else
		exec (":" . w:helplines)
	endif
endfunction " >>>

"" ChangeTop() - change top dir to cursor dir
function! s:ChangeTop() " <<<
	let ln = line(".")
  let l = getline(ln)

	" on current top or non-tree line?
	if l !~ '^[| `]'
		return
	endif

	" parent dir
	if l =~ '^\.\. '
		call s:ChdirUp()
		return
	endif

	let curfile = s:GetAbsPath2(ln, 0)
	if curfile !~ '/$'
		let curfile = substitute (curfile, '[^/]*$', "", "")
	endif
	call s:InitWithDir (curfile)
endfunction " >>>

"" RecursiveExpand() - expand cursor dir recursively
function! s:RecursiveExpand() " <<<
	echo "recursively expanding, this might take a while (CTRL-C to stop)"

	let curfile = s:GetAbsPath2(line("."), 0)

	if w:firstdirline == 0
		let init_ln = w:helplines
		let curfile = substitute (getline (init_ln), '[ {]*', "", "")
	else
		let init_ln = w:firstdirline
	endif

	let init_ind = match (getline (init_ln), '[^-| `]') / 4

	let curfile = substitute (curfile, '[^/]*$', "", "")

	let l = getline (init_ln)

	if l =~ ' {{{$'
		if foldclosed (init_ln) != -1
			foldopen
		endif
	endif

	if l !~ ' {{{$' " dir not open
		call s:ReadDir (init_ln, curfile)

		if getline (init_ln) !~ ' {{{$' " dir still not open (empty)
			echo "expansion done"
			return
		endif
	endif

	let ln = init_ln + 1

	let l = getline (ln)

	let match_str = '[^-| `]'
	while init_ind < (match (l, '[^-| `]') / 4)
		let tl = l
		let tln = ln
		let ln = ln + 1
		let l = getline (ln)

		if tl =~ ' {{{$'
			if foldclosed (tln) != -1
				foldopen
			endif
			continue
		endif

		" link or non dir
		if tl =~ ' -> ' || tl !~ '/[ }]*$'
			continue
		endif

		let curfile = s:GetAbsPath2(tln, 0)

		call s:ReadDir (tln, curfile)

		let l = getline (ln)
	endwhile

	exec (":" . init_ln)
	echo "expansion done"
endfunction " >>>

"" OpenExplorer() - open file explorer on cursor dir
function! s:OpenExplorer() " <<<
	let curfile = s:GetAbsPath2 (line ("."), 0)

	if w:firstdirline == 0
		let curfile = getcwd ()
	else
		" remove file name, if any
		let curfile = substitute (curfile, '[^/]*$', "", "")
	endif

	let curfile = escape (curfile, w:escape_chars)

	let oldwin = winnr()
	wincmd p
	if oldwin == winnr() || &modified
		wincmd p
		exec ("new " . curfile)
	else
		exec ("edit " . curfile)
	endif

endfunction " >>>

"" Activate() - (un)fold read dirs, read unread dirs, open files, cd .. on ..
function! s:Activate() " <<<
	let ln = line(".")
  let l = getline(ln)

	" parent dir, change to it
  if l =~ '^\.\. (up a directory)$'
		call s:ChdirUp()
    return
  endif

	" directory loaded, toggle folded state
	if l =~ ' {{{$'
		if foldclosed(ln) == -1
			foldclose
		else
			foldopen
		endif
		return
	endif

	" on top, no folds, or not on tree
	if l !~ '^[-| `]'
		return
	endif

	" get path of line
	let curfile = s:GetAbsPath2 (ln, 0)

	if curfile =~ '/$' " dir
	  call s:ReadDir (ln, curfile)
		return
	else " file
		let f = escape (curfile, w:escape_chars)
		let oldwin = winnr()
		wincmd p
		if oldwin == winnr() || (&modified && s:BufInWindows(winbufnr(winnr())) < 2)
			wincmd p
			exec ("new " . f)
		else
			exec ("edit " . f)
		endif
	endif
endfunction " >>>

"" RefreshDir() - refresh current dir
function! s:RefreshDir() " <<<
	let curfile = s:GetAbsPath2(line("."), 0)

	let init_ln = w:firstdirline

	" not in tree, or on path line or parent is top
	if curfile == "" || init_ln == 0
		call s:InitWithDir("")
		return
	endif

	let save_l = @l

	" remove file name, if any
	let curfile = substitute (curfile, '[^/]*$', "", "")

	let @l = getline (init_ln)

	" if there is no fold, just do normal ReadDir, and return
	if @l !~ ' {{{$'
		call s:ReadDir (init_ln, curfile)
		return
	endif

	" TODO factor

	if foldclosed(init_ln) == -1
		foldclose
	endif

	" remove one foldlevel from line
	let @l = substitute (@l, ' {{{$', "", "")

	exec (":" . init_ln)

	setlocal modifiable
	silent normal ddk
	silent put l
	setlocal nomodifiable

	call s:ReadDir (init_ln, curfile)

	let @l = save_l
endfunction " >>>

"" ToggleHiddenFiles() - toggle hidden files
function! s:ToggleHiddenFiles() " <<<
	let w:hidden_files = w:hidden_files ? 0 : 1
	let msg = w:hidden_files ? "on" : "off"
	let msg = "hidden files now = " . msg
	echo msg
	call s:UpdateHeader ()
	call s:RefreshDir()
endfunction " >>>

" toggle dir sorting
function! s:ToggleDirSort() " <<<
	if w:dirsort == 0
		let w:dirsort = 1
		let msg = "dirs first"
	elseif w:dirsort > 0
		let w:dirsort = -1
		let msg = "dirs last"
	else
		let w:dirsort = 0
		let msg = "off"
	endif
	let msg = "dirs sorting now = " . msg
	echo msg
	call s:UpdateHeader ()
	call s:RefreshDir()
endfunction " >>>

"" StartShell() - start shell in cursor dir
function! s:StartShell() " <<<
	let ln = line(".")

	let curfile = s:GetAbsPath2 (ln, 1)
	let prevdir = getcwd()

	if w:firstdirline == 0
		let dir = prevdir
	else
		let dir = substitute (curfile, '[^/]*$', "", "")
	endif

	execute "lcd " . escape (dir, w:escape_chars)
	shell
	execute "lcd " . escape (prevdir, w:escape_chars)
endfunction " >>>

"" GetAbsPath2() -  get absolute path at line ln, set w:firstdirline,
""  - if ignore_current is 1, don't set line to current line when on a dir
function! s:GetAbsPath2(ln,ignore_current) " <<<
	let lnum = a:ln
	let l = getline(lnum)

	let w:firstdirline = 0

	" in case called from outside the tree
	if l =~ '^[/".]' || l =~ '^$'
		return ""
	endif

	let wasdir = 0

	" strip file
	let curfile = substitute (l,'^[-| `]*',"","") " remove tree parts
	let curfile = substitute (curfile,'[ {}]*$',"",'') " remove fold marks
	"let curfile = substitute (curfile,'[*=@|]$',"","") " remove file class

	" remove leading escape
	let curfile = substitute (curfile,'^\\', "", "")

	if curfile =~ '/$' && a:ignore_current == 0
		let wasdir = 1
		let w:firstdirline = lnum
	endif

	let curfile = substitute (curfile,' -> .*',"","") " remove link to
	if wasdir == 1
		let curfile = substitute (curfile, '/\?$', '/', "")
	endif

	let indent = match(l,'[^-| `]') / 4

	let dir = ""
	while lnum > 0
		let lnum = lnum - 1
		let lp = getline(lnum)
		if lp =~ '^/'
			let sd = substitute (lp, '[ {]*$', "", "")
			let dir = sd . dir
			break
		endif
		if lp =~ ' {{{$'
			let lpindent = match(lp,'[^-| `]') / 4
			if lpindent < indent
				if w:firstdirline == 0
					let w:firstdirline = lnum
				endif
				let indent = indent - 1
				let sd = substitute (lp, '^[-| `]*',"","") " rm tree parts
				let sd = substitute (sd, '[ {}]*$', "", "") " rm foldmarks
				let sd = substitute (sd, ' -> .*','/',"") " replace link to with /

				" remove leading escape
				let sd = substitute (sd,'^\\', "", "")

				let dir = sd . dir
				continue
			endif
		endif
	endwhile
	let curfile = dir . curfile
	return curfile
endfunction " >>>

"" ToggleHelp() - toggle between long and short help
function! s:ToggleHelp() " <<<
	let w:helplines = (w:helplines <= 4) ? 6 : 0
	call s:UpdateHeader ()
endfunction " >>>

"" Determine the number of windows open to this buffer number.
"" Care of Yegappan Lakshman.  Thanks!
fun! s:BufInWindows(bnum) " <<<
  let cnt = 0
  let winnum = 1
  while 1
    let bufnum = winbufnr(winnum)
    if bufnum < 0
      break
    endif
    if bufnum == a:bnum
      let cnt = cnt + 1
    endif
    let winnum = winnum + 1
  endwhile

  return cnt
endfunction " >>>

"" UpdateHeader() - update the header
function! s:UpdateHeader() " <<<
	let oldRep=&report
	set report=10000
	normal! mt

  " Remove old header
  0
	setlocal modifiable | silent! 1,/^" ?/ d _ | setlocal nomodifiable

  call s:AddHeader()

	" return to previous mark
  0
  if line("'t") != 0
    normal! `t
  endif

  let &report=oldRep
endfunction " >>>

"" - AddHeader() -  add the header with help information
function! s:AddHeader() " <<<
	if w:dirsort == 0
		let dt = "off)\n"
	elseif w:dirsort == 1
		let dt = "dirs first)\n"
	else
		let dt = "dirs last)\n"
	endif

	let save_f=@f
	1
	let ln = 3
	if w:helplines > 4
		let ln=ln+1 | let @f=   "\" <ret> = same as 'o' below\n"
		let ln=ln+1 | let @f=@f."\" o     = (file) open in another window\n"
		let ln=ln+1 | let @f=@f."\" o     = (dir) toggle dir fold or load dir\n"
		let ln=ln+1 | let @f=@f."\" X     = recursive expand cursor dir\n"
		let ln=ln+1 | let @f=@f."\" E     = open Explorer on cursor dir\n"
		let ln=ln+1 | let @f=@f."\" C     = chdir top of tree to cursor dir\n"
		let ln=ln+1 | let @f=@f."\" H     = chdir top of tree to home dir\n"
		let ln=ln+1 | let @f=@f."\" u     = chdir top of tree to parent dir\n"
		let ln=ln+1 | let @f=@f."\" :CD d = chdir top of tree to dir <d>\n"
		let ln=ln+1 | let @f=@f."\" p     = move cursor to parent dir\n"
		let ln=ln+1 | let @f=@f."\" r     = refresh cursor dir\n"
		let ln=ln+1 | let @f=@f."\" R     = refresh top dir\n"
		let ln=ln+1 | let @f=@f."\" S     = start a shell in cursor dir\n"
		let ln=ln+1 | let @f=@f."\" :Yank = yank <range> lines withoug fold marks\n"
		let ln=ln+1 | let @f=@f."\" D     = toggle dir sort (now = " . dt
		let ln=ln+1 | let @f=@f."\" a     = toggle hidden files (now = "
					\ . ((w:hidden_files) ? "on)\n" : "off)\n")
		let ln=ln+1 | let @f=@f."\" ?     = toggle long help\n"
	else
		let ln=ln+1 | let @f="\" ? : toggle long help\n"
	endif
	let w:helplines = ln

	setlocal modifiable | silent put! f | setlocal nomodifiable

	let @f=save_f
endfunction " >>>

let &cpo = s:cpo_save

" vim: set ts=2 sw=2 foldmethod=marker foldmarker=<<<,>>> foldlevel=2 :
