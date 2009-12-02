syntax clear

syn match       notesBullet         "^[-*]\s" display
hi! def link    notesBullet         Special

syn match       notesLegacyKey      "^\w\+\.\s\s\+"  display
hi! def link    notesLegacyKey      notesFileKey

syn match       notesFileDefine     "^[^:]\+:\(\s.\+\)\?$" display contains=notesFileKey,notesFileDefineOp,notesFileDefineVal

syn match       notesFileKey        "^[^:]\+" contained display
hi! def link    notesFileKey        Label

syn match       notesFileDefineOp   ":" contained display
hi! def link    notesFileDefineOp   Type

syn match       notesFileDefineVal  ":\@<=.*$" contained display
hi! def link    notesFileDefineVal  Normal

syn match       notesFileSubHeading "^\s*\[[^\]]\+\]\s*$" display
hi! def link    notesFileSubHeading String

syn match       notesFileHeading    "^=\+\s.\+$" display
hi! def link    notesFileHeading    Function

syn match       notesFileVerbatim   "^\s\+[^#].*$" display
hi! def link    notesFileVerbatim   PreProc

syn match       notesFileComment    "^\s*#.*$" display contains=notesTODO,notesXXX
hi! def link    notesFileComment    Comment

syn keyword     notesTODO           TODO contained
hi! def link    notesTODO           Todo

syn keyword     notesXXX            XXX contained
hi! def link    notesXXX            Todo

set foldenable
set foldlevel=0
set foldmethod=expr
set foldnestmax=1
set foldexpr=getline(v:lnum)=~\"^=\"?'>1':getline(v:lnum)=~\"^##\"?0:1
set foldtext=v:folddashes.substitute(getline(v:foldstart),'=','','g')

let b:current_syntax = "notesfile"

