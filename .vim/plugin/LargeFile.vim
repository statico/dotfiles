" LargeFile: Sets up an autocmd to make editing large files work with celerity
"   Author:		Charles E. Campbell, Jr.
"   Date:		Sep 23, 2008
"   Version:	4
" GetLatestVimScripts: 1506 1 :AutoInstall: LargeFile.vim

" ---------------------------------------------------------------------
" Load Once: {{{1
if exists("g:loaded_LargeFile") || &cp
 finish
endif
let g:loaded_LargeFile = "v4"
let s:keepcpo          = &cpo
set cpo&vim

" ---------------------------------------------------------------------
" Commands: {{{1
com! Unlarge			call s:Unlarge()
com! -bang Large		call s:LargeFile(<bang>0,expand("%"))

" ---------------------------------------------------------------------
"  Options: {{{1
if !exists("g:LargeFile")
 let g:LargeFile= 20	" in megabytes
endif

" ---------------------------------------------------------------------
"  LargeFile Autocmd: {{{1
" for large files: turns undo, syntax highlighting, undo off etc
" (based on vimtip#611)
augroup LargeFile
 au!
 au BufReadPre * call <SID>LargeFile(0,expand("<afile>"))
 au BufReadPost *
 \  if &ch < 2 && (getfsize(expand("<afile>")) >= g:LargeFile*1024*1024 || getfsize(expand("<afile>")) == -2)
 \|  echomsg "***note*** handling a large file"
 \| endif
augroup END

" ---------------------------------------------------------------------
" s:LargeFile: {{{2
fun! s:LargeFile(force,fname)
"  call Dfunc("LargeFile(force=".a:force." fname<".a:fname.">)")
  if a:force || getfsize(a:fname) >= g:LargeFile*1024*1024 || getfsize(a:fname) <= -2
   syn clear
   let b:eikeep = &ei
   let b:ulkeep = &ul
   let b:bhkeep = &bh
   let b:fdmkeep= &fdm
   let b:swfkeep= &swf
   set ei=FileType
   setlocal noswf bh=unload fdm=manual ul=-1
   let fname=escape(substitute(a:fname,'\','/','g'),' ')
   exe "au LargeFile BufEnter ".fname." set ul=-1"
   exe "au LargeFile BufLeave ".fname." let &ul=".b:ulkeep."|set ei=".b:eikeep
   exe "au LargeFile BufUnload ".fname." au! LargeFile * ". fname
   echomsg "***note*** handling a large file"
  endif
"  call Dret("s:LargeFile")
endfun

" ---------------------------------------------------------------------
" s:Unlarge: this function will undo what the LargeFile autocmd does {{{2
fun! s:Unlarge()
"  call Dfunc("s:Unlarge()")
  if exists("b:eikeep") |let &ei  = b:eikeep |endif
  if exists("b:ulkeep") |let &ul  = b:ulkeep |endif
  if exists("b:bhkeep") |let &bh  = b:bhkeep |endif
  if exists("b:fdmkeep")|let &fdm = b:fdmkeep|endif
  if exists("b:swfkeep")|let &swf = b:swfkeep|endif
  syn on
  doau FileType
"  call Dret("s:Unlarge")
endfun

" ---------------------------------------------------------------------
"  Restore: {{{1
let &cpo= s:keepcpo
unlet s:keepcpo
" vim: ts=4 fdm=marker
