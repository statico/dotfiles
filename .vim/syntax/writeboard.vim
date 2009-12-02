" Vim syntax file
" Language:     Writeboard
" Maintainers:  Ian Langworth <ian.langworth@gmail.com>
"

" TODO remove when releasing
syntax clear 
if version < 600
  syntax clear
"elseif exists("b:current_syntax")
"  finish
endif

syn cluster wbStylers contains=wbSTrong,wbEmphasize

syn match wbHead            "^h[12]\.\s\+.*$"
syn match wbIndent          "^\s.*$" 
syn match wbItem            "^+\?[*#]\+" contains=@wbStylers
syn match wbStrong          "^\@<![^\w]\@=\*.\{-1,}\*[^\w]\@=" 
syn match wbEmphasize       "^\@<![^\w\/]\@=_.\{-1,}_[^\w]\@=" 
syn match wbImage           "^\@<![^\w\/]\@=!.\{-1,}![^\w]\@="
syn match wbLink            "\"[^\"]\":http://[^\s]\+"

if version >= 508 || !exists("did_writeboard_syntax_inits")
  if version < 508
    let did_spork_syntax_inits = 1
    command -nargs=+ HiLink hi link <args>
  else
    " TODO - remove ! below
    command -nargs=+ HiLink hi! def link <args>
  endif

  HiLink wbHead                 Function
  HiLink wbIndent               PreProc
  HiLink wbItem                 Label 

  HiLink wbImage                Identifier 
  HiLink wbLink                 Identifier 

  hi def wbStrong    term=bold cterm=bold gui=bold
  hi def wbEmphasize term=italic cterm=italic gui=italic

  delcommand HiLink
endif

let b:current_syntax = "writeboard"

