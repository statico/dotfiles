" $Id: CVSAnnotate.vim,v 1.5 2002/10/01 21:34:02 rhiestan Exp $
" Vim syntax file
" Language:	CVS annotate output
" Maintainer:	Bob Hiestand <bob@hiestandfamily.org>
" Last Change:	$Date: 2002/10/01 21:34:02 $
" Remark:	Used by the cvscommand plugin.  Originally written by Mathieu
" Clabaut
if version < 600
  syntax clear
elseif exists("b:current_syntax")
  finish
endif

syn match cvsDate 	/\d\d-...-\d\d/ 		contained
syn match cvsName  	/(\S* /hs=s+1,he=e-1 		contained nextgroup=cvsDate
syn match cvsVer 	/^\d\+\(\.\d\+\)\+/ 		contained nextgroup=cvsName
syn region cvsHead 	start="^\d\+\.\d\+" end="):" 	contains=cvsVer,cvsName,cvsDate

if !exists("did_cvsannotate_syntax_inits")
let did_cvsannotate_syntax_inits = 1
hi link cvsText 	String
hi link cvsDate 	Comment
hi link cvsName	Type
hi link cvsVer	Statement
endif

let b:current_syntax="CVSAnnotate"
