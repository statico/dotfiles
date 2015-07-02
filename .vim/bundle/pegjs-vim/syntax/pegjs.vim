" Vim syntax file
" Language: PEG JS Grammars
" Maintainer: Andrew Lunny <alunny@gmail.com>
" Latest Revision: 15 June 2012
" License: MIT

if exists("b:current_syntax")
    finish
endif

syn include @js syntax/javascript.vim

syn match pegOperator "[/.+*?&!]"
syn region charSet start="\[" end="\]"
syn match rule "[a-zA-Z$_][a-zA-Z$_0-9]*"
syn match ruleDef "[_a-zA-Z$][a-zA-Z$_0-9]*" contained
syn match equals "=" contained
syn match initialize "[a-zA-Z$_][a-zA-Z$_0-9]*[\n\t ]*\".*\"[\n\t ]*=" contains=ruleDef,equals,innerLiteral
syn match initialize "[a-zA-Z$_][a-zA-Z$_0-9]*[\n\t ]*'.*'[\n\t ]*=" contains=ruleDef,equals,innerLiteral
syn match initialize "[a-zA-Z$_][a-zA-Z$_0-9]*[\n\t ]*=" contains=ruleDef,equals
syn match exprLabel "[a-zA-Z$_][a-zA-Z$_0-9]*:"he=e-1
syn region literal start="'" end="'"
syn region literal start="\"" end="\""
syn region innerLiteral start="'" end="'" contained
syn region innerLiteral start="\"" end="\"" contained
syn region comment start="/[*]" end="[*]/"
syn region comment start="//" end="\n"

syn region jsBlock start="{" end="}" keepend contains=@js

hi def link ruleDef         PreProc
hi def link rule            Type
hi def link exprLabel       Identifier
hi def link literal         String
hi def link pegOperator     Operator
hi def link charSet         Operator
hi def link whitespace      PreProc
hi def link innerLiteral    Comment
