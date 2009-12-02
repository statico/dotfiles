" Vim syntax file
" Language:     Subversion (svn) commit file
" Maintainer:   Dmitry Vasiliev <dima@hlabs.spb.ru>
" URL:          http://www.hlabs.spb.ru/vim/svn.vim
" Last Change:  $Date: 2003/06/10 11:55:20 $
" $Revision: 1.4 $

" For version 5.x: Clear all syntax items.
" For version 6.x: Quit when a syntax file was already loaded.
if version < 600
  syntax clear
elseif exists("b:current_syntax")
  finish
endif

syntax clear

syn match svnLogRemoved    "^D      .*$" 
syn match svnLogAdded      "^A[ M]  .*$"
syn match svnLogModified   "^M[ M]  .*$"
syn match svnLogProperty   "^_M     .*$"

" Define the default highlighting.
" For version 5.7 and earlier: only when not done already.
" For version 5.8 and later: only when an item doesn't have highlighting yet.
if version >= 508 || !exists("did_svn_syn_inits")
  if version <= 508
    let did_svn_syn_inits = 1
    command -nargs=+ HiLink hi link <args>
  else
    command -nargs=+ HiLink hi def link <args>
  endif

  HiLink svnRegion      Comment
  HiLink svnLogRemoved     Constant
  HiLink svnLogAdded       Identifier
  HiLink svnLogModified    Special
  HiLink svnLogProperty    Special

  delcommand HiLink
endif

let b:current_syntax = "svnlog"
