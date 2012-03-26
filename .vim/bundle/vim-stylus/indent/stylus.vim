" Vim indent file
" Language: Stylus
" Maintainer: Marc Harter
" Last Change: 2010 May 21
" Based On: sass.vim from Tim Pope

if exists("b:did_indent")
  finish
endif
unlet! b:did_indent
let b:did_indent = 1

setlocal indentexpr=GetStylusIndent()
setlocal indentkeys=o,O,*<Return>,},],0),!^F
setlocal formatoptions+=r

if exists("*GetStylusIndent")  " only define once
  finish
endif

function! GetStylusIndent()
  let lnum     = prevnonblank(v:lnum-1)
  if lnum == 0
    return 0
  endif
  let line     = substitute(getline(lnum),'[\s()]\+$','','')  " get last line strip ending whitespace
  let cline    = substitute(substitute(getline(v:lnum),'\s\+$','',''),'^\s\+','','')  " get current line, trimmed
  let lastcol  = strlen(line)  " get last col in prev line
  let line     = substitute(line,'^\s\+','','')  " then remove preceeding whitespace
  let indent   = indent(lnum)  " get indent on prev line
  let cindent  = indent(v:lnum)  " get indent on current line
  let increase = indent + &sw  " increase indent by the shift width
  if indent   == indent(lnum)
    let indent = cindent <= indent ? indent : increase
  endif

  let group = synIDattr(synID(lnum,lastcol,1),'name')

  " for debugging only
  echo group

  " if group !~? 'css.*' && line =~? ')\s*$' " match user functions
  "   return increase
  if group =~? '\v^%(cssTagName|cssClassName|cssIdentifier|cssSelectorOp|cssSelectorOp2|cssBraces|cssAttributeSelector|cssPseudoClass|cssPseudoClassId|stylusId|stylusClass)$'
    return increase
  elseif (group == 'stylusUserFunction') && (indent(lnum) == '0') " mixin definition
    return increase
  else
    return indent
  endif
endfunction

" vim:set sw=2;
