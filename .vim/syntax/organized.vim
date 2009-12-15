" Vim syntax file
" Language:     Organized Plain Text
" Maintainers:  Ian Langworth <ian.langworth@gmail.com>

if version < 600
  syntax clear
endif

syn match organizedSeparator    "^-\+$"
syn match organizedSeparator    "^_\+$"
syn match organizedHeading      "^[^a-z]\+$"
syn match organizedDirective    "^[^a-z]\+:"

if version >= 508 || !exists("did_organized_syntax_inits")
  if version < 508
    let did_organized_syntax_inits = 1
    command -nargs=+ HiLink hi link <args>
  else
    " TODO - remove ! below
    command -nargs=+ HiLink hi! def link <args>
  endif

  HiLink organizedSeparator     Special
  HiLink organizedHeading       Title
  HiLink organizedDirective     Statement

  delcommand HiLink
endif

let b:current_syntax = "organized"

