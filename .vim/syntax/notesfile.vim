syntax clear 

syn match       notesFileDefine     "^[^:]\+:.\+$" contains=notesFileKey,notesFileDefineOp

syn match       notesFileKey        "^[^:]\+" contained
hi! def link    notesFileKey        Label

syn match       notesFileDefineOp   ":" contained
hi! def link    notesFileDefineOp   Operator

syn match       notesFileHeading    "^=\+ .\+$"
hi! def link    notesFileHeading    Function

syn match       notesFileComment    "^#.*$"
hi! def link    notesFileComment    Comment

set foldenable 
set foldlevel=0 
set foldmethod=expr 
set foldnestmax=1
set foldexpr=getline(v:lnum)=~\"^=\"?'>1':getline(v:lnum)=~\"^#\"?0:1
set foldtext=v:folddashes.substitute(getline(v:foldstart),'=','','g')

let b:current_syntax = "notesfile"

