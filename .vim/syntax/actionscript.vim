" Vim syntax file
" Language:     ActionScript
" Maintainer:   Manish Jethani <manish.jethani@gmail.com>
" URL:          http://geocities.com/manish_jethani/actionscript.vim
" Last Change:  2006 June 26

if exists("b:current_syntax")
  finish
endif

syn region  asStringDQ	      start=+"+  skip=+\\\\\|\\"+  end=+"+
syn region  asStringSQ	      start=+'+  skip=+\\\\\|\\'+  end=+'+
syn match   asNumber          "-\=\<\d\+L\=\>\|0[xX][0-9a-fA-F]\+\>"
syn region  asRegExp          start=+/+ skip=+\\\\\|\\/+ end=+/[gismx]\?\s*$+ end=+/[gismx]\?\s*[;,)]+me=e-1 oneline
" TODO: E4X

syn keyword asCommentTodo     TODO FIXME XXX TBD contained

syn match   asComment         "//.*$" contains=asCommentTodo
syn region  asComment         start="/\*"  end="\*/" contains=asCommentTodo

syn keyword asDirective       import include
syn match   asDirective       "\<use\s\+namespace\>"

syn keyword asAttribute       public private internal protected override final dynamic native static

syn keyword asDefinition      const var class extends interface implements package namespace
syn match   asDefinition        "\<function\(\s\+[gs]et\)\?\>"

syn keyword asGlobal          NaN Infinity undefined eval parseInt parseFloat isNaN isFinite decodeURI decodeURIComponent encodeURI encodeURIComponent

syn keyword asType            Object Function Array String Boolean Number Date Error XML
syn keyword asType            int uint void *

syn keyword asStatement       if else do while for with switch case default continue break return throw try catch finally
syn match   asStatement       "\<for\s\+each\>"

syn keyword asIdentifier      super this

syn keyword asConstant        null true false
syn keyword asOperator        new in is as typeof instanceof delete

syn match   asBraces          "[{}]"

" Flex metadata
syn keyword asMetadataTag     Bindable DefaultProperty Effect Event Exclude IconFile MaxChildren ResourceBundle Style contained
syn match   asMetadata        "^\s*\[.*" contains=asMetadataTag,asStringDQ,asComment

syn sync fromstart
syn sync maxlines=300

hi def link asStringDQ        String
hi def link asStringSQ        String
hi def link asNumber          Number
hi def link asRegExp          Special
hi def link asCommentTodo     Todo
hi def link asComment         Comment
hi def link asDirective       Include
hi def link asAttribute       Define
hi def link asDefinition      Structure
hi def link asGlobal          Macro
hi def link asType            Type
hi def link asStatement       Statement
hi def link asIdentifier      Identifier
hi def link asConstant        Constant
hi def link asOperator        Operator
hi def link asBraces          Function
hi def link asMetadataTag     PreProc

let b:current_syntax = "actionscript"

" vim: ts=8
