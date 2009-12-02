" pf syntax file
" Language:	OpenBSD packet filter configuration (pf.conf)
" Maintainer:	Camiel Dobbelaar <cd@sentia.nl>
" Last Change:	2003 Feb 24

" For version 5.x: Clear all syntax items
" For version 6.x: Quit when a syntax file was already loaded
if version < 600
  syntax clear
elseif exists("b:current_syntax")
  finish
endif

setlocal iskeyword+=$
setlocal iskeyword+=-
setlocal iskeyword+=.
setlocal iskeyword+=/
setlocal iskeyword+=:
setlocal iskeyword+=<
setlocal iskeyword+=>
setlocal iskeyword+=_

setlocal foldmethod=syntax
syn sync fromstart

syn cluster	pfNotLS		contains=pfComment,pfTodo,pfVarAssign
syn keyword	pfCmd		altq antispoof binat block nat pass queue rdr scrub table set
syn keyword	pfService	auth bgp domain finger ftp ftp-data http https
syn keyword	pfService	ident imap irc isakmp kerberos mail nameserver
syn keyword	pfService	nfs nntp ntp pop3 portmap pptp rpcbind rsync
syn keyword	pfService	smtp snmp snmp-trap snmptrap socks ssh sunrpc
syn keyword	pfService	syslog telnet tftp www
syn keyword	pfTodo		TODO XXX contained
syn keyword	pfWildAddr	all any
syn match	pfComment	/#.*$/ contains=pfTodo
syn match	pfCont		/\\$/
syn match	pfErrClose	/}/
syn match	pfIPv4		/\<\d\{1,3}\.\d\{1,3}\.\d\{1,3}\.\d\{1,3}\>/
syn match	pfIPv4Masked	/\<[0-9.]\+\/\d\+\>/ contains=pfNetmask
syn match	pfIPv6		/\<[a-fA-F0-9:]\+:[a-fA-F0-9:.]\+\>/
syn match	pfNetmask	/\/\d\+\>/
syn match	pfNum		/\<\d\+\>/
syn match	pfTable		/\<<\s*[a-zA-Z][a-zA-Z0-9_]*\s*>\>/
syn match	pfTodo		/\<TODO:/me=e-1 contained
syn match	pfTodo		/\<XXX:/me=e-1 contained
syn match	pfVar		/\<$[a-zA-Z][a-zA-Z0-9_]*\>/
syn match	pfVarAssign	/^\s*[a-zA-Z][a-zA-Z0-9_]*\s*=/me=e-1
syn region	pfFold1		start=/^#\{1}>/ end=/^#\{1,3}>/me=s-1 transparent fold
syn region	pfFold2		start=/^#\{2}>/ end=/^#\{2,3}>/me=s-1 transparent fold
syn region	pfFold3		start=/^#\{3}>/ end=/^#\{3}>/me=s-1 transparent fold
syn region	pfList		start=/{/ end=/}/ transparent contains=ALLBUT,pfErrClose,@pfNotLS
syn region	pfString	start=/"/ end=/"/ transparent contains=ALLBUT,pfString,@pfNotLS
syn region	pfString	start=/'/ end=/'/ transparent contains=ALLBUT,pfString,@pfNotLS

" Define the default highlighting.
" For version 5.7 and earlier: only when not done already
" For version 5.8 and later: only when an item doesn't have highlighting yet
if version >= 508 || !exists("did_c_syn_inits")
  if version < 508
    let did_c_syn_inits = 1
    command -nargs=+ HiLink hi link <args>
  else
    command -nargs=+ HiLink hi def link <args>
  endif

  HiLink pfCmd		Statement
  HiLink pfComment	Comment
  HiLink pfCont		Statement
  HiLink pfErrClose	Error
  HiLink pfIPv4		Type
  HiLink pfIPv4Masked	Type
  HiLink pfIPv6		Type
  HiLink pfNetmask	Constant
  HiLink pfNum		Constant
  HiLink pfService	Constant
  HiLink pfTable	Identifier
  HiLink pfTodo		Todo
  HiLink pfVar		Identifier
  HiLink pfVarAssign	Identifier
  HiLink pfWildAddr	Type

  delcommand HiLink
endif

let b:current_syntax = "pf"
