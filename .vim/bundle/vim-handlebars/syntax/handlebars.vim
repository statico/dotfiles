" Handlebars syntax
" Language:    Handlebars
" Maintainer:  Bruno Michel <brmichel@free.fr>
" Last Change: Jun 23th, 2011
" Version:	   0.1
" URL:         https://github.com/nono/vim-handlebars


" Read the HTML syntax to start with
if version < 600
  so <sfile>:p:h/html.vim
else
  runtime! syntax/html.vim
  unlet b:current_syntax
  syntax clear Javascript
endif

if version < 600
  syntax clear
elseif exists("b:current_syntax")
  finish
endif

" Standard HiLink will not work with included syntax files
if version < 508
  command! -nargs=+ HtmlHiLink hi link <args>
else
  command! -nargs=+ HtmlHiLink hi def link <args>
endif


syn match   hbsError            /}}}\?/
syn match   hbsInsideError      /{{[{#<>=!\/]\?/   containedin=@hbsInside

syn cluster htmlHbsContainer   add=htmlHead,htmlTitle,htmlString,htmlH1,htmlH2,htmlH3,htmlH4,htmlH5,htmlH6
syn region  hbsInside          start=/{{/ end=/}}/  keepend transparent containedin=@htmlHbsContainer

syn match   hbsHandlebars      "{{\|}}"                                 containedin=hbsInside
syn match   hbsUnescape        "{{{\|}}}"                               containedin=hbsInside
syn match   hbsOperators       "=\|\.\|/"                               containedin=hbsInside

syn region  hbsSection         start="{{[#/]"lc=2 end=/}}/me=e-2        containedin=hbsInside
syn region  hbsPartial         start=/{{[<>]/lc=2 end=/}}/me=e-2        containedin=hbsInside
syn region  hbsMarkerSet       start=/{{=/lc=2    end=/=}}/me=e-2       containedin=hbsInside

syn region  hbsComment         start=/{{!/          end=/}}/me=e-2      containedin=htmlHead contains=Todo
syn region  hbsQString         start=/'/ skip=/\\'/ end=/'/             containedin=hbsInside
syn region  hbsDQString        start=/"/ skip=/\\"/ end=/"/             containedin=hbsInside

syn match   hbsConditionals    "\([/#]\(if\|unless\)\|else\)"           containedin=hbsInside
syn match   hbsHelpers         "[/#]\(with\|each\)"                     containedin=hbsInside


" Define the default highlighting.
" For version 5.7 and earlier: only when not done already
" For version 5.8 and later: only when an item doesn't have highlighting yet
if version >= 508 || !exists("did_lisp_syntax_inits")
  if version < 508
    let did_lisp_syntax_inits = 1
    command -nargs=+ HiLink hi link <args>
  else
    command -nargs=+ HiLink hi def link <args>
  endif

  HtmlHiLink hbsError         Error
  HtmlHiLink hbsInsideError   Error

  HtmlHiLink hbsHandlebars    Identifier
  HtmlHiLink hbsUnescape      Special
  HtmlHiLink hbsOperators     Operator

  HtmlHiLink hbsConditionals  Conditional
  HtmlHiLink hbsHelpers       Repeat

  HtmlHiLink hbsSection       Number
  HtmlHiLink hbsPartial       Include
  HtmlHiLink hbsMarkerSet     Number

  HtmlHiLink hbsComment       Comment
  HtmlHiLink hbsQString       String
  HtmlHiLink hbsDQString      String

  delcommand HiLink
endif


let b:current_syntax = 'handlebars'
