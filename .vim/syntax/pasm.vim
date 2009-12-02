" Vim syntax file
" Language:	Parrot Assembler
" Maintainer:	Scott Beck <scott@gossamer-threads.com>
" Last Change:	2002 Feb 28

" For version 5.x: Clear all syntax items
" For version 6.x: Quit when a syntax file was already loaded
if version < 600
  syntax clear
elseif exists("b:current_syntax")
  finish
endif

syn include @Pod syntax/pod.vim
syn region myPod start="^=pod" start="^=item" start="^=head" end="^=cut" keepend contains=@Pod

syn match pasmIdentifier "[A-Za-z0-9_]\+"
syn match pasmLabel      "[A-Za-z0-9_]\+:"he=e-1
syn match pasmComment    "#.*"

" Only have 32 registers
syn match pasmINT "I\(\([1-2][0-9]\)\|\(3[0-2]\)\|[1-9]\)"
syn match pasmSTR "S\(\([1-2][0-9]\)\|\(3[0-2]\)\|[1-9]\)"
syn match pasmFLT "N\(\([1-2][0-9]\)\|\(3[0-2]\)\|[1-9]\)"
syn match pasmPMC "P\(\([1-2][0-9]\)\|\(3[0-2]\)\|[1-9]\)"

syn match pasmNumber "[0-9]\+"
syn match pasmString +"[^"]\+"+

syn keyword pasmOpBasic         end noop
syn keyword pasmOpSystem        close err open readline ord print read time write
syn keyword pasmOpRegLoad       set set_keyed clone
syn keyword pasmOpCond          eq ne lt le gt ge if unless
syn keyword pasmOpArith         abs add cmod dec div inc mod mul not pow sub 
syn keyword pasmOpString        chopm concat repeat length substr 
syn keyword pasmOpMath          acos asec asin atan cos cosh exp ln log10 log2 sec sech sin sinh tan tanh
syn keyword pasmOpBit           and not or shl shr xor
syn keyword pasmOpFlags         debug bounds profile trace
syn keyword pasmOpReg           cleari clearn clearp clears popi popn popp pops pushi pushn pushp pushs
syn keyword pasmOpRegStack      entrytype save restore rotate_up
syn keyword pasmOpCtrlFlow      branch bsr jsr jump
syn keyword pasmOpSymbolTbl     find_global 
syn keyword pasmOpMisc          newinterp runinterp new find_type ret sleep setline getline setfile getfile setpackage getpackage warningson warningsoff

" Define the default highlighting.
" For version 5.7 and earlier: only when not done already
" For version 5.8 and later: only when an item doesn't have highlighting yet
if version >= 508 || !exists("did_pasm_syntax_inits")
  if version < 508
    let did_pasm_syntax_inits = 1
    command -nargs=+ HiLink hi link <args>
  else
    command -nargs=+ HiLink hi def link <args>
  endif

  HiLink pasmLabel       Label
  HiLink pasmIdentifier  Identifier
  HiLink pasmINT         Type
  HiLink pasmSTR         Type
  HiLink pasmFLT         Type
  HiLink pasmPMC         Type
  HiLink pasmNumber      Number
  HiLink pasmString      String
  HiLink pasmComment     Comment
  HiLink pasmOp          Statement
  HiLink pasmOpBasic     pasmOp
  HiLink pasmOpSystem    pasmOp
  HiLink pasmOpRegLoad   pasmOp
  HiLink pasmOpCond      pasmOp
  HiLink pasmOpArith     pasmOp
  HiLink pasmOpString    pasmOp
  HiLink pasmOpMath      pasmOp
  HiLink pasmOpBit       pasmOp
  HiLink pasmOpFlags     pasmOp
  HiLink pasmOpReg       pasmOp
  HiLink pasmOpRegStack  pasmOp
  HiLink pasmOpCtrlFlow  pasmOp
  HiLink pasmOpSymbolTbl pasmOp
  HiLink pasmOpMisc      pasmOp

  delcommand HiLink
endif

let b:current_syntax = "pasm"

" vim: ts=8

