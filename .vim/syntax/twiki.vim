"============================================================================
"
" TWiki syntax file
"
" Language:    TWiki
" Version:     $Id: twiki.vim,v 1.6 2006/03/03 08:44:25 rat Exp $
" Maintainer:  Rainer Thierfelder <rainer{AT}rainers-welt{DOT}de>
" Additions:   Eric Haarbauer <ehaar{DOT}com{AT}grithix{DOT}dyndns{DOT}org>
" License:     GPL (http://www.gnu.org/licenses/gpl.txt)
"    Copyright (C) 2004  Rainer Thierfelder
"
"    This program is free software; you can redistribute it and/or modify
"    it under the terms of the GNU General Public License as published by
"    the Free Software Foundation; either version 2 of the License, or
"    (at your option) any later version.
"
"    This program is distributed in the hope that it will be useful,
"    but WITHOUT ANY WARRANTY; without even the implied warranty of
"    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
"    GNU General Public License for more details.
"
"    You should have received a copy of the GNU General Public License
"    along with this program; if not, write to the Free Software
"    Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301 USA
"
"============================================================================
"
" For version 5.x: Clear all syntax items
" For version 6.x: Quit when a syntax file was already loaded
if !exists("main_syntax")
  if version < 600
    syntax clear
  elseif exists("b:current_syntax")
    finish
  endif
  let main_syntax = 'twiki'
endif

" Don't use standard HiLink, it will not work with included syntax files
if version < 508
  command! -nargs=+ TwikiHiLink   highlight link <args>
  command! -nargs=+ TwikiSynColor highlight <args>
else
  command! -nargs=+ TwikiHiLink   highlight default link <args>
  command! -nargs=+ TwikiSynColor highlight default <args>
endif

"============================================================================
" Group Definitions:    {{{1
"============================================================================

syntax match twikiSeparator    "^---\+"
syntax match twikiBulletedList "^\(   \)\+\*\ze "
syntax match twikiOrderedList  "^\(   \)\+1\ze "

syntax match twikiVariable "\([^!]\|^\)\zs%\w\+%"
syntax match twikiTag      "<\w\+>"

syntax match twikiDelimiter "|"

syntax region twikiComment  start="<!--" end="-->"
syntax region twikiVerbatim matchgroup=twikiTag
    \ start="<verbatim>" end="</verbatim>"
syntax region twikiPre matchgroup=twikiTag
    \ start="<pre>" end="</pre>"

syntax region twikiHeading matchgroup=twikiHeadingMarker oneline
    \ start="^---+\+" end="$"

let s:wikiWord = '\u[a-z0-9]\+\(\u[a-z0-9]\+\)\+'

execute 'syntax match twikiAnchor +^#'.s:wikiWord.'\ze\(\>\|_\)+'
execute 'syntax match twikiWord +\(\s\|^\)\zs\(\u\l\+\.\)\='.s:wikiWord.'\(#'.s:wikiWord.'\)\=\ze\(\>\|_\)+'
" Regex guide:                   ^pre        ^web name       ^wikiword  ^anchor               ^ post

" Links: {{{2
syntax region twikiLink matchgroup=twikiLinkMarker
    \ start="\( \|^\)\zs\[\[" end="\]\]\ze\([,. ?):-]\|$\)"
    \ contains=twikiForcedLink,twikiLinkRef keepend

execute 'syntax match twikiForcedLink +[ A-Za-z0-9]\+\(#'.s:wikiWord.'\)\=+ contained'

syntax match twikiLinkRef    ".\{-}\ze\]\["
    \ contained contains=twikiLinkMarker nextgroup=twikiLinkLabel
syntax match twikiLinkLabel  ".\{-}\ze\]\]"   contained contains=twikiLinkMarker
syntax match twikiLinkMarker "\]\["           contained

" Emphasis:  {{{2
function! s:TwikiCreateEmphasis(token, name)
    execute 'syntax region twiki'.a:name.
           \' oneline start=+\(^\|[ ]\)\zs'.a:token.
           \'+ end=+'.a:token.'\ze\([,. ?):-]\|$\)+'
endfunction

call s:TwikiCreateEmphasis('=',  'Fixed')
call s:TwikiCreateEmphasis('==', 'BoldFixed')
call s:TwikiCreateEmphasis('\*', 'Bold')
call s:TwikiCreateEmphasis('_',  'Italic')
call s:TwikiCreateEmphasis('__', 'BoldItalic')

"============================================================================
" Group Linking:    {{{1
"============================================================================

TwikiHiLink twikiHeading       String
TwikiHiLink twikiHeadingMarker Operator
TwikiHiLink twikiVariable      PreProc
TwikiHiLink twikiTag           PreProc
TwikiHiLink twikiComment       Comment
TwikiHiLink twikiWord          Tag
TwikiHiLink twikiAnchor        PreProc
TwikiHiLink twikiVerbatim      Constant
TwikiHiLink twikiPre           Constant
TwikiHiLink twikiBulletedList  Operator
TwikiHiLink twikiOrderedList   Operator

TwikiHiLink twikiDelimiter     Operator

" Links
TwikiSynColor twikiLinkMarker term=bold cterm=bold gui=bold
TwikiHiLink   twikiForcedLink Tag
TwikiHiLink   twikiLinkRef    Tag
TwikiHiLink   twikiLinkLabel  Identifier

" Emphasis
TwikiSynColor twikiFixed      term=underline cterm=underline gui=underline
TwikiSynColor twikiBoldFixed  term=bold,underline cterm=bold,underline gui=bold,underline
TwikiSynColor twikiItalic     term=italic cterm=italic gui=italic
TwikiSynColor twikiBoldItalic term=bold,italic cterm=bold,italic gui=bold,italic
TwikiSynColor twikiBold       term=bold cterm=bold gui=bold

"============================================================================
" Clean Up:    {{{1
"============================================================================

delcommand TwikiHiLink
delcommand TwikiSynColor

if main_syntax == 'twiki'
  unlet main_syntax
endif

let b:current_syntax = "twiki"

" vim:fdm=marker
