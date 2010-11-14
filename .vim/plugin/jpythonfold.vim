" Fold routines for python code, version 3.2
" Source: http://www.vim.org/scripts/script.php?script_id=2527
" Last Change: 2009 Feb 25
" Author: Jurjen Bos
" Bug fixes and helpful comments: Grissiom, David Froger, Andrew McNabb

" Principles:
" - a def/class starts a fold
" a line with indent less than the previous def/class ends a fold
" empty lines and comment lines are linked to the previous fold
" comment lines outside a def/class are never folded
" other lines outside a def/class are folded together as a group
" for algorithm, see bottom of script

" - optionally, you can get empty lines between folds, see (***)
" - another option is to ignore non-python files see (**)
" - you can also modify the def/class check,
"    allowing for multiline def and class definitions see (*)

" Note for vim 7 users:
" Vim 6 line numbers always take 8 columns, while vim 7 has a numberwidth variable
" you can change the 8 below to &numberwidth if you have vim 7,
" this is only really useful when you plan to use more than 8 columns (i.e. never)

" Note for masochists trying to read this:
" I wanted to keep the functions short, so I replaced occurences of
" if condition
"     statement
" by
" if condition | statement
" wherever I found that useful

" (*)
" class definitions are supposed to ontain a colon on the same line.
" function definitions are *not* required to have a colon, to allow for multiline defs.
" I you disagree, use instead of the pattern '^\s*\(class\s.*:\|def\s\)'
" to enforce : for defs:                     '^\s*\(class\|def\)\s.*:'
" you'll have to do this in two places.
let s:defpat = '^\s*\(@\|class\s.*:\|def\s\)'

" (**) Ignore non-python files
" Commented out because some python files are not recognized by Vim
if &filetype != 'python'
    finish
endif

setlocal foldmethod=expr
setlocal foldexpr=GetPythonFold(v:lnum)
setlocal foldtext=PythonFoldText()

function! PythonFoldText()
  let fs = v:foldstart
  while getline(fs) =~ '^\s*@' | let fs = nextnonblank(fs + 1)
  endwhile
  let line = getline(fs)
  let nnum = nextnonblank(fs + 1)
  let nextline = getline(nnum)
  "get the document string: next line is ''' or """
  if nextline =~ "^\\s\\+[\"']\\{3}\\s*$"
      let line = line . " " . matchstr(getline(nextnonblank(nnum + 1)), '^\s*\zs.*\ze$')
  "next line starts with qoutes, and has text
  elseif nextline =~ "^\\s\\+[\"']\\{1,3}"
      let line = line." ".matchstr(nextline, "^\\s\\+[\"']\\{1,3}\\zs.\\{-}\\ze['\"]\\{0,3}$")
  elseif nextline =~ '^\s\+pass\s*$'
    let line = line . ' pass'
  endif
  "compute the width of the visible part of the window (see Note above)
  let w = winwidth(0) - &foldcolumn - (&number ? 8 : 0)
  let size = 1 + v:foldend - v:foldstart
  "compute expansion string
  let spcs = '................'
  while strlen(spcs) < w | let spcs = spcs . spcs
  endwhile
  "expand tabs (mail me if you have tabstop>10)
  let onetab = strpart('          ', 0, &tabstop)
  let line = substitute(line, '\t', onetab, 'g')
  return strpart(line.spcs, 0, w-strlen(size)-7).'.'.size.' lines'
endfunction

function! GetBlockIndent(lnum)
    " Auxiliary function; determines the indent level of the surrounding def/class
    " "global" lines are level 0, first def &shiftwidth, and so on
    " scan backwards for class/def that is shallower or equal
    let ind = 100
    let p = a:lnum+1
    while indent(p) >= 0
        let p = p - 1
        " skip empty and comment lines
        if getline(p) =~ '^$\|^\s*#' | continue
        " zero-level regular line
        elseif indent(p) == 0 | return 0
        " skip deeper or equal lines
        elseif indent(p) >= ind || getline(p) =~ '^$\|^\s*#' | continue
        " indent is strictly less at this point: check for def/class
        elseif getline(p) =~ s:defpat && getline(p) !~ '^\s*@'
            " level is one more than this def/class
            return indent(p) + &shiftwidth
        endif
        " shallower line that is neither class nor def: continue search at new level
        let ind = indent(p)
    endwhile
    "beginning of file
    return 0
endfunction

" Clever debug code, use as: call PrintIfCount(n,"Line: ".a:lnum.", value: ".x)
let s:counter=0
function! PrintIfCount(n,t)
    "Print text the nth time this function is called
    let s:counter = s:counter+1
    if s:counter==a:n | echo a:t
    endif
endfunction

function! GetPythonFold(lnum)
    " Determine folding level in Python source (see "higher foldlevel theory" below)
    let line = getline(a:lnum)
    let ind = indent(a:lnum)
    " Case D***: class and def start a fold
    " If previous line is @, it is not the first
    if line =~ s:defpat && getline(prevnonblank(a:lnum-1)) !~ '^\s*@'
        " let's see if this range of 0 or more @'s end in a class/def
        let n = a:lnum
        while getline(n) =~ '^\s*@' | let n = nextnonblank(n + 1)
        endwhile
        " yes, we have a match: this is the first of a real def/class with decorators
        if getline(n) =~ s:defpat
            return ">".(ind/&shiftwidth+1)
        endif
    " Case E***: empty lines fold with previous
    " (***) change '=' to -1 if you want empty lines/comment out of a fold
    elseif line == '' | return '='
    endif
    " now we need the indent from previous
    let p = prevnonblank(a:lnum-1)
    while p>0 && getline(p) =~ '^\s*#' | let p = prevnonblank(p-1)
    endwhile
    let pind = indent(p)
    " If previous was definition: count as one level deeper
    if getline(p) =~ s:defpat && getline(prevnonblank(a:lnum - 1)) !~ '^\s*@'
        let pind = pind + &shiftwidth
    " if begin of file: take zero
    elseif p==0 | let pind = 0
    endif
    " Case S*=* and C*=*: indent equal
    if ind>0 && ind==pind | return '='
    " Case S*>* and C*>*: indent increase
    elseif ind>pind | return '='
    " All cases with 0 indent
    elseif ind==0
        " Case C*=0*: separate global code blocks
        if pind==0 && line =~ '^#' | return 0
        " Case S*<0* and S*=0*: global code
        elseif line !~'^#'
            " Case S*<0*: new global statement if/while/for/try/with
            if 0<pind && line!~'^else\s*:\|^except.*:\|^elif.*:\|^finally\s*:' | return '>1'
            " Case S*=0*, after level 0 comment
            elseif 0==pind && getline(prevnonblank(a:lnum-1)) =~ '^\s*#' | return '>1'
            " Case S*=0*, other, stay 1
            else | return '='
            endif
        endif
        " Case C*<0= and C*<0<: compute next indent
        let n = nextnonblank(a:lnum+1)
        while n>0 && getline(n) =~'^\s*#' | let n = nextnonblank(n+1)
        endwhile
        " Case C*<0=: split definitions
        if indent(n)==0 | return 0
        " Case C*<0<: shallow comment
        else | return -1
        end
    endif
    " now we really need to compute the actual fold indent
    " do the hard computation
    let blockindent = GetBlockIndent(a:lnum)
    " Case SG<* and CG<*: global code, level 1
    if blockindent==0 | return 1
    endif
    " now we need the indent from next
    let n = nextnonblank(a:lnum+1)
    while n>0 && getline(n) =~'^\s*#' | let n = nextnonblank(n+1)
    endwhile
    let nind = indent(n)
    " Case CR<= and CR<>
    "if line !~ '^\s*#' | call PrintIfCount(4,"Line: ".a:lnum.", blockindent: ".blockindent.", n: ".n.", nind: ".nind.", p: ".p.", pind: ".pind)
    endif
    if line =~ '^\s*#' && ind>=nind | return -1
    " Case CR<<: return next indent
    elseif line =~ '^\s*#' | return nind / &shiftwidth
    " Case SR<*: return actual indent
    else | return blockindent / &shiftwidth
    endif
endfunction

" higher foldlevel theory
" There are five kinds of statements: S (code), D (def/class), E (empty), C (comment)

" Note that a decorator statement (beginning with @) counts as definition,
" but that of a sequence of @,@,@,def only the first one counts
" This means that a definiion only counts if not preceded by a decorator

" There are two kinds of folds: R (regular), G (global statements)

" There are five indent situations with respect to the previous non-emtpy non-comment line:
" > (indent), < (dedent), = (same); < and = combine with 0 (indent is zero)
" Note: if the previous line is class/def, its indent is interpreted as one higher

" There are three indent situations with respect to the next (non-E non-C) line:
" > (dedent), < (indent), = (same)

" Situations (in order of the script):
" stat  fold prev   next
" SDEC  RG   ><=00  ><=
" D     *    *      *     begin fold level if previous is not @: '>'.ind/&sw+1
" E     *    *      *     keep with previous: '='
" S     *    =      *     stays the same: '='
" C     *    =      *     combine with previous: '='
" S     *    >      *     stays the same: '='
" C     *    >      *     combine with previous: '='
" C     *    =0     *     separate blocks: 0
" S     *    <0     *     becomes new level 1: >1 (except except/else: 1)
" S     *    =0     *     stays 1: '=' (after level 0 comment: '>1')
" C     *    <0     =     split definitions: 0
" C     *    <0     <     shallow comment: -1
" C     *    <0     >     [never occurs]
" S     G    <      *     global, not the first: 1
" C     G    <      *     indent isn't 0: 1
" C     R    <      =     foldlevel as computed for next line: -1
" C     R    <      >     foldlevel as computed for next line: -1
" S     R    <      *     compute foldlevel the hard way: use function
" C     R    <      <     foldlevel as computed for this line: use function
