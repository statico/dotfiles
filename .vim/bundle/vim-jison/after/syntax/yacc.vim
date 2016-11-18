" Vim syntax file
" Language:	Yacc
" Maintainer:	Charles E. Campbell, Jr. <NdrOchipS@PcampbellAfamily.Mbiz>
" Last Change:	Aug 12, 2010
" Version:	9
" URL:	http://mysite.verizon.net/astronaut/vim/index.html#vimlinks_syntax
"
" Options: {{{1
"   g:yacc_uses_cpp : if this variable exists, then C++ is loaded rather than C

" ---------------------------------------------------------------------
" this version of syntax/yacc.vim requires 6.0 or later
if version < 600
 finish
endif
if exists("b:current_syntax")
 syntax clear
endif

" ---------------------------------------------------------------------
"  Folding Support {{{1
if has("folding")
 com! -nargs=+ HiFold	<args> fold
else
 com! -nargs=+ HiFold	<args>
endif

" ---------------------------------------------------------------------
" Read the C syntax to start with {{{1
" TODO: Shouldn't we autodetect this?
if exists("g:yacc_uses_cpp")
 syn include @yaccCode	syntax/cpp.vim
 syn cluster yaccStructure contains=cStructure,cppStructure
else
 syn include @yaccCode	syntax/c.vim
 syn cluster yaccStructure contains=cStructure
endif

" ---------------------------------------------------------------------
"  Yacc Clusters: {{{1
syn cluster yaccInitCluster	contains=yaccInitKey,yaccBrkt,yaccType,yaccString,yaccUnionStart,yaccHeader,yaccComment,yaccDefines,yaccParseParam,yaccParseOption,bisonOption,bisonPrologueStart,bisonInitialActionStart
syn cluster yaccRulesCluster	contains=yaccNonterminal,yaccString,yaccComment

" ---------------------------------------------------------------------
"  Yacc Sections: {{{1
HiFold syn	region	yaccInit	start='\%^\_.'ms=s-1,rs=s-1	matchgroup=yaccSectionSep	end='^%%'me=e-2,re=e-2	contains=@yaccInitCluster	nextgroup=yaccRules	skipwhite skipempty 
HiFold syn	region	yaccRules	matchgroup=yaccSectionSep	start='^%%'		end='^%%'me=e-2,re=e-2	contains=@yaccRulesCluster	nextgroup=yaccEndCode	skipwhite skipempty contained
HiFold syn	region	yaccEndCode	matchgroup=yaccSectionSep	start='^%%'		end='\%$'		contains=@yaccCode	contained

" ---------------------------------------------------------------------
" Yacc Commands: {{{1

HiFold syn	region	yaccHeader	matchgroup=yaccSep	start="^\s*\zs%{"	end="^\s*%}"	contained	contains=@yaccCode

syn	match	yaccDefines	'^%define\s\+.*$'	contained
syn	match	yaccParseParam	'%\(parse\|lex\)-param\>'	contained	skipwhite
syn	match	yaccParseOption '%\%(api\.pure\|pure-parser\|locations\|error-verbose\)\>'	contained
syn	region	yaccParseParamStr	matchgroup=Delimiter	start='{'	end='}'	contained	contains=@yaccStructure	

syn	match	yaccDelim	"[:|]"	contained
syn	match	yaccOper	"@\d\+"	contained

syn	match	yaccInitKey	"^\s*%\(token\|type\|left\|right\|start\|ident\|nonassoc\)\>"	contained
syn	match	yaccKey	"\s%\(prec\|expect\)\>"	contained
syn	match	yaccKey	"\$\(<[a-zA-Z_][a-zA-Z_0-9]*>\)\=[\$0-9]\+"	contained	containedin=cParen
syn	match	yaccKey	"@[\$0-9]\+"	contained	containedin=cParen
syn	keyword	yaccKeyActn	yyerrok yyclearin	contained	containedin=cParen
syn cluster yaccCode add=yaccKey,yaccKeyActn

syn	match	bisonOption	"^\s*%\(require\|defines\|debug\|skeleton\|name-prefix\)\>"	contained

syn	match	yaccUnionStart	"^%union"	skipwhite skipnl nextgroup=yaccUnion	contained
HiFold syn	region	yaccUnion	matchgroup=yaccCurly start="{" matchgroup=yaccCurly end="}" contains=@yaccCode	contained
syn	match	yaccBrkt	"[<>]"	contained
syn	match	yaccType	"<[a-zA-Z_][a-zA-Z0-9_]*>"	contains=yaccBrkt	contained

HiFold syn	region	yaccNonterminal	start="^\s*\a\(\w\|\w\.\w\)*\ze\_s*\(/\*\_.\{-}\*/\)\=\_s*:"	matchgroup=yaccDelim end=";"	matchgroup=yaccSectionSep end='^%%$'me=e-2,re=e-2 contains=yaccAction,yaccDelim,yaccString,yaccComment	contained
syn	region	yaccComment	start="/\*"	end="\*/"	contained
syn	match	yaccString	"'[^']*'"	contained
syn	match	yaccString	'"[^"]*"'	contained

syn	match	bisonPrologueStart	"^%code\%\(\s\+\%\(requires\|provides\|top\)\)\="	nextgroup=bisonPrologue	skipwhite	skipnl	contained
HiFold	syn	region	bisonPrologue	matchgroup=yaccCurly	start="{"	matchgroup=yaccCurly	end="}"	contains=@yaccCode	contained

syn match	bisonInitialActionStart	"^%initial-action"	nextgroup=bisonInitialAction	skipwhite	skipnl	contained
HiFold	syn	region	bisonInitialAction	matchgroup=yaccCurly	start="{"	matchgroup=yaccCurly	end="}"	contains=@yaccCode	contained

" ---------------------------------------------------------------------
" I'd really like to highlight just the outer {}.  Any suggestions??? {{{1
syn	match	yaccCurlyError	"[{}]"
HiFold syn	region	yaccAction	matchgroup=yaccCurly start="{" end="}" contains=@yaccCode	contained

" ---------------------------------------------------------------------
" Yacc synchronization: {{{1
syn sync fromstart

" ---------------------------------------------------------------------
" Define the default highlighting. {{{1
if !exists("did_yacc_syn_inits")
  command -nargs=+ HiLink hi def link <args>

  " Internal yacc highlighting links {{{2
  HiLink yaccBrkt	yaccStmt
  HiLink yaccInitKey	yaccStmt
  HiLink yaccKey	yaccStmt
  HiLink yaccOper	yaccStmt
  HiLink yaccUnionStart	yaccKey
  HiLink bisonPrologueStart	yaccKey
  HiLink bisonInitialActionStart	yaccKey

  " External yacc highlighting links {{{2
  HiLink yaccComment	Comment
  HiLink yaccCurly	Delimiter
  HiLink yaccCurlyError	Error
  HiLink yaccDefines	cDefine
  HiLink yaccParseParam	yaccParseOption
  HiLink yaccParseOption	cDefine
  HiLink yaccNonterminal	Function
  HiLink yaccDelim	Delimiter
  HiLink yaccKeyActn	Special
  HiLink yaccSectionSep	Todo
  HiLink yaccSep	Delimiter
  HiLink yaccString	String
  HiLink yaccStmt	Statement
  HiLink yaccType	Type
  HiLink bisonOption	yaccParseOption

  " since Bram doesn't like my Delimiter :| {{{2
  HiLink Delimiter	Type

  delcommand HiLink
endif

" ---------------------------------------------------------------------
"  Cleanup: {{{1
delcommand HiFold
let b:current_syntax = "yacc"

" ---------------------------------------------------------------------
"  Modelines: {{{1
" vim: ts=15 fdm=marker
