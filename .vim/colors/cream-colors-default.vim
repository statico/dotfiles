"======================================================================
" cream-colors-default.vim
"
" Cream -- An easy-to-use configuration of the famous Vim text editor
" [ http://cream.sourceforge.net ] Copyright (C) 2002-2004  Steve Hall
" 
" License:
" This program is free software; you can redistribute it and/or modify
" it under the terms of the GNU General Public License as published by
" the Free Software Foundation; either version 2 of the License, or
" (at your option) any later version.
" [ http://www.gnu.org/licenses/gpl.html ]
" 
" This program is distributed in the hope that it will be useful, but
" WITHOUT ANY WARRANTY; without even the implied warranty of
" MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
" General Public License for more details.
" 
" You should have received a copy of the GNU General Public License
" along with this program; if not, write to the Free Software
" Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
" 02111-1307, USA.
"

set background=light
highlight clear
if exists("syntax_on")
  syntax reset
endif

"let g:colors_name = "cream-default"

highlight LineNr gui=NONE guifg=#999999 guibg=#e8e8e8 cterm=NONE ctermfg=DarkGray ctermbg=LightGray


"+++ Cream:

" invisible characters
highlight NonText         guifg=#bbbbbb gui=none
highlight SpecialKey      guifg=#bbbbbb gui=none

" statusline
highlight User1  gui=bold guifg=#bbbbbb guibg=#f3f3f3
highlight User2  gui=bold guifg=#000000 guibg=#f3f3f3
highlight User3  gui=bold guifg=#0000ff guibg=#f3f3f3
highlight User4  gui=bold guifg=#ff0000 guibg=#f3f3f3

" bookmarks
highlight Cream_ShowMarksHL gui=bold guifg=blue guibg=lightblue ctermfg=blue ctermbg=lightblue cterm=bold

" spell check
highlight BadWord gui=bold guifg=DarkBlue guibg=#ffdddd ctermfg=black ctermbg=lightblue

" current line
highlight CurrentLine term=reverse ctermbg=0 ctermfg=14 gui=none guibg=#ffffcc

" email
highlight EQuote1 guifg=#0000cc
highlight EQuote2 guifg=#6666cc
highlight EQuote3 guifg=#9999cc
highlight Sig guifg=#999999

"+++

