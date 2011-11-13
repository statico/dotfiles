" Vim syntax file
syntax clear " XXX TODO remove

if version < 600
  syntax clear
elseif exists("b:current_syntax")
  finish
endif

syn cluster kwikiStylers contains=kwikiStrong,kwikiEmphasize,kwikiUnderline,kwikiInline,kwikiDelete,kwikiDash

syn match waflPhrase    /{\w\+\:\s\+.*}/ contains=waflPhraseStart,waflName,waflPhraseEnd
syn match waflName      /\w\+:/ contained
syn match waflPhraseStart /{/ contained
syn match waflPhraseEnd   /{/ contained

syn match kwikiLine    /^----\+\s*$/ 
syn match kwikiHead    /^=\{1,6}\s\+.*$/
syn match kwikiPre     /^\s.*$/
syn match kwikiComment /^#\s.*$/
syn match kwikiItem    /^[*0]\+\s\+.*$/ contains=@kwikiStylers
syn match KwikiTable   /^|.*|$/ contains=@kwikiStylers 
syn match kwikiStrong       /[^\w]\@=\*.*\*[^\w]\@=/ contains=@kwikiStylers
syn match kwikiEmphasize    /[^\w]\@=\/.*\/[^\w]\@=/ contains=@kwikiStylers
syn match kwikiUnderline    /[^\w]\@=_.*_[^\w]\@=/ contains=@kwikiStylers
syn match kwikiInline       /[^\w]\@=\[=.*\][^\w]\@=/ contains=@kwikiStylers
syn match kwikiDelete       /[^\w]\@=-.*-[^\w]\@=/ contains=@kwikiStylers

syntax match kwikiDash  /---\?/

" asis

"syn region kwikiLink    start="\[\{1,2}" end="\]\{1,2}"
" url, title
" forcedlink
" hyperlink
" titledhyperlin
" wikilink
" titledwikilink
" maillink
" titledmaillink

" Define the default highlighting.
" For version 5.7 and earlier: only when not done already
" For version 5.8 and later: only when an item doesn't have highlighting yet
if version >= 508 || !exists("did_wiki_syntax_inits")
  if version < 508
    let did_kwiki_syntax_inits = 1
    command -nargs=+ HiLink hi link <args>
  else
    command -nargs=+ HiLink hi def link <args>
  endif

  HiLink waflPhrase             Function
  HiLink waflName               Type
  HiLink waflStartPhrase        Function
  HiLink waflEndPhrase          Function

  HiLink kwikiLine              NonText
  HiLink kwikiHead              Function
  HiLink kwikiPre               PreProc
  HiLink kwikiComment           Comment
  HiLink kwikiItem              Label 

  HiLink kwikiTable             NonText

  HiLink kwikiIndent            Number
  HiLink kwikiDef               Special
  HiLink kwikiLink              Identifier 

  HiLink kwikiDash              NonText

  hi def kwikiStrong    term=bold cterm=bold gui=bold
  hi def kwikiEmphasize term=italic cterm=italic gui=italic
  hi def kwikiUnderline term=underline cterm=underline gui=underline
  HiLink kwikiInline    PreProc
  hi def kwikiDelete    term=standout cterm=standout gui=standout

  delcommand HiLink
endif

let b:current_syntax = "kwiki"

