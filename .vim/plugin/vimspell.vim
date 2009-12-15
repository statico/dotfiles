"$Id: vimspell.vim,v 1.95 2005/01/04 17:47:53 clabaut Exp $
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Name:		    vimspell
" Description:	    Use ispell or aspell to highlight spelling errors on the
"		    fly, or on demand.
" Author:	    Mathieu Clabaut <mathieu.clabaut at free.fr>
" Original Author:  Claudio Fleiner <claudio at fleiner.com>
" Maintainer:	    Mathieu Clabaut <mathieu.clabaut at free.fr>
" Url:		    http://www.vim.org/scripts/script.php?script_id=465
"
" GetLatestVimScripts: 465 2966 vimspell.vim
"
" Last Change:	    04-Jan-2005.
"
" Licence:	    This program is free software; you can redistribute it
"                   and/or modify it under the terms of the GNU General Public
"                   License.  See http://www.gnu.org/copyleft/gpl.txt
"
" Credits:	    Claudio Fleiner <claudio at fleiner.com> for the original
"		      script,
"		    Matthias Veit <matthias_veit at yahoo.de> for
"		      implementation idea of fly spelling.
"		    Bob Hiestand <bob at hiestandfamily.org> for his
"		      cvscommand.vim script, which was a reference for
"		      documentation, vim usage, some ideas and functions.
"		    Peter Valach <pvalach at gmx.net> for suggestions, bug
"		      corrections, and vim conformance tip.
"		    Markus Braun <Markus.Braun at krawel.de> for several bug
"		      report and patches :-). He helped me in reducing the
"		      TODO list and in doing early testing before each
"		      release.
"		    Tim Allen <firstlight at redneck.gacracker.org> for showing
"		      me a way to autogenerate help file in his 'posting'
"		      script.
"		    Mikolaj Machowski <mikmach at wp.pl> for implementation of
"		      on the fly spell checking in insert mode.
"		    Guo-Peng Wen for several patches, improvements and code
"		      cleaning.
"		    Hari Krishna Dara who shows me how to map a key which 
"		      calls a function without leaving the insert mode.
"		    Hugo Haas who suggests the automatic language guessing 
"		      function, and provides me with a sample function.
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
"
" Section: Documentation 
"----------------------------
"
" Documentation should be available by ":help vimspell" command, once the
" script has been copied in you .vim/plugin directory.
"
" You still can read the documentation at the end of this file. Locate it by
" searching the "vimspell-contents" string (and set ft=help to have
" appropriate syntaxic coloration). 
"
" Section: Plugin header {{{1
" loaded_vimspell is set to 1 when the initialization begins, and 2 when it
" completes.  This allows various actions to only be taken by functions after
" system initialization.

" Exit quickly when already loaded or when 'compatible' is set.
if exists("loaded_vimspell") || &compatible
   finish
endif
" Line continuation used here
let s:cpo_save = &cpo
set cpo&vim

" Use a trick to get the <SID> number of this script (from Vim
" documentation).
map <SID>xx <SID>xx
let s:sid=substitute(maparg("<SID>xx"),"xx","","")
unmap <SID>xx


let loaded_vimspell = 1
"causes a bug with iskeyword usage when encoding=utf-8
"scriptencoding iso-8859-15

" Filetype, spell checker and/or language dependents default options {{{2

let s:spell_ispell_tex_args   = "-t"
let s:spell_ispell_html_args  = "-H"
let s:spell_ispell_sgml_args  = "-H"
let s:spell_ispell_nroff_args = "-n"

let s:spell_aspell_tex_args   = "--mode=tex"
let s:spell_aspell_html_args  = "--mode=sgml"
let s:spell_aspell_sgml_args  = "--mode=sgml"
let s:spell_aspell_mail_args  = "--mode=email"

" In french for example, "l'objet" or "½uvre" are words recognized by ispell.
" add "'", "e in o" and "e in a"  to 'iskeyword'
" 'there should probably be some encoding consideration here... ? )
"
let s:spell_francais_iskeyword  =  "@,48-57,_,192-255,39,½,æ,¼,Æ,-"
let s:spell_russian_iskeyword = "@,48-57,_,192-255,^a-z,^A-Z" 
let s:spell_english_iskeyword = "@,48-57,_,192-255,^192-255"

" The double quote is used in latex for umlauts (which are written like "a,
" "o, "u in latex)
let s:spell_german_tex_iskeyword  = "@,48-57,192-255,=,\,34"

let s:known_spellchecker = "aspell,ispell"

if has("win32") || has("win16")
  let s:grep="find "
  let s:find="dir "
  let s:findname=" "
  let s:findopt=" "
else
  let s:grep="grep "
  let s:find="find "
  let s:findname=" -name "
  let s:findopt=" -print -type f "
endif



" Section: Utility functions {{{1
"
" Function: s:SpellMenuAlternatives() {{{2
" Propose alternative for keyword under cursor. Build popup menu to provide
" alternatives.
function! s:SpellMenuAlternatives()
  call s:SpellCheckLanguage(1)

  let l:alternatives=s:SpellSystem("echo \"".escape(expand("<cword>"),'\"')."\" | "
	\ . b:spell_executable . b:spell_options . " -a -d ".b:spell_language)

    " add \n, that the next substitute works
  let l:alternatives=substitute(l:alternatives, "^", "\n", "g")
    " delete all irrelevant lines
  let l:alternatives=substitute(l:alternatives, "\n[@*#][^\n]*", "", "g")
    " join lines
  let l:alternatives=substitute(l:alternatives, "\n", "", "g")
    " delete irrelevant begin of alternatives
  let l:alternatives=substitute(l:alternatives, "^.*: \\(.*\\)", "\\1, ", "")

  let l:alterNr=1
  let l:alter=""
  let l:index=stridx(l:alternatives, ", ")

  while (l:index > 0 && l:alterNr <= 20)
    let l:oneAlter=strpart(l:alternatives, 0, l:index)
    let l:alternatives=strpart(l:alternatives, l:index+2)
    let l:index=stridx(l:alternatives, ", ")
    let l:oneAlter=escape(l:oneAlter,'"')

  let alter=alter."amenu <silent> 1.5 PopUp.Spell.".l:oneAlter
	  \ ." :call <SID>SpellReplace(\"".l:oneAlter."\")<CR> |"
    let l:alterNr=l:alterNr+1
  endwhile

  :silent! exe "aunmenu PopUp.Spell"
  if l:alter !=? ""
    exe l:alter
  "else
    "amenu <silent> 1.5 PopUp.Spell  :echo "no alternatives"<CR>
  endif
endfunction


" Function: s:SpellProposeAlternatives() {{{2
" Propose alternative for keyword under cursor. Define mapping used to correct
" the word under the cursor.
function! s:SpellProposeAlternatives()
  " If auto spelling is not enable, we should ensure that SpellSetupBuffer was
  " called in order to work properly.
  if !s:enable_autocommand
    call s:SpellSetupBuffer()
  endif

  call s:SpellCheckLanguage(1)

  let l:alternatives=s:SpellSystem("echo \"".escape(expand("<cword>"),'\"')."\" | "
	\ . b:spell_executable . b:spell_options . " -a -d ".b:spell_language)

    " add \n, that the next substitute works
  let l:alternatives=substitute(l:alternatives, "^", "\n", "g")
    " delete all irrelevant lines
  let l:alternatives=substitute(l:alternatives, "\n[@*#][^\n]*", "", "g")
    " join lines
  let l:alternatives=substitute(l:alternatives, "\n", "", "g")
    " delete irrelevant begin of alternatives
  let l:alternatives=substitute(l:alternatives, "^.*: \\(.*\\)", "\\1, ", "")

  let l:alterValue=","
  let l:alterNr=1
  let l:alter=""
  let l:index=stridx(l:alternatives, ", ")

  while (l:index > 0 && l:alterNr <= 20)
    let l:oneAlter=strpart(l:alternatives, 0, l:index)
    let l:alternatives=strpart(l:alternatives, l:index+2)
    let l:index=stridx(l:alternatives, ", ")
    let l:oneAlter=escape(l:oneAlter,'"')

    let alter=alter."echo \"".l:alterNr.": ".l:oneAlter."\"|"
    let l:alterValue=l:alterValue.l:alterNr.": ".l:oneAlter.","

    let l:alterNr=l:alterNr+1
  endwhile

  let l:replaced=0

  if l:alter !=? ""
    echo "Checking ".expand("<cword>")
	  \.": Type 0 for no change, *<number> to replace all or :"
    exe l:alter
    echo ">"
    let c=getchar()
    if nr2char(c) == '*'
      let replace="call s:SpellReplaceEverywhere(word)"
      let c=getchar()
    else
      let replace="call s:SpellReplace(word)"
    endif
    if c != 0
      let c=nr2char(c)
      if c == '0'
	return 0
      elseif '1' <= c && c <= '9'
	if l:alterNr > 10 " l:alterNr == 10 means 9 alternatives
	  let ch=c
	  let c=getchar()
	  let c=nr2char(c)
	  if '0' <= c && c <= '9'
	    let c=ch . c
	  else
	    let c=ch
	  endif
	endif
	let word=substitute(l:alterValue,'^.*,'.c.': \([^,]\+\),.*', '\1','')
	if word != l:alterValue
	  silent exe replace
	  let l:replaced=1
	endif
	redraw
      endif
    endif
  else
    echo "no alternatives"
  endif
  if l:replaced && b:spell_auto_jump 
     call s:SpellNextError(1)
  endif
endfunction


" Function: s:SpellReplace() {{{2
" Replace word under cursor by the string given in parameter
function! s:SpellReplace(s)
  exe "normal ciw".a:s."\<esc>"
endfunction

" Function: s:SpellReplaceEverywhere() {{{2
" Replace word under cursor by the string given in parameter, in the whole
" text.
function! s:SpellReplaceEverywhere(s)
  exe ":%s/\\C\\<".expand("<cword>")."\\>/".a:s."/g"
  normal 
endfunction

" Function: s:SpellContextMapping() {{{2
" Define mapping defined for spell checking.
function! s:SpellContextMapping()
  execute "map <silent> <buffer> "
	\ . s:SpellGetOption("spell_case_accept_map","<Leader>si")
	\ . " :call <SID>SpellCaseAccept()<CR><C-L>"
  execute "map <silent> <buffer> "
	\ . s:SpellGetOption("spell_accept_map","<Leader>su")
	\ . " :call <SID>SpellAccept()<CR><C-L>"
  execute "map <silent> <buffer> "
	\ . s:SpellGetOption("spell_ignore_map","<Leader>sa")
	\ . " :call <SID>SpellIgnore()<CR><C-L>"
  execute "map <silent> <buffer> "
	\ . s:SpellGetOption("spell_exit_map","<Leader>sq")
	\ . " :let @_=<SID>SpellAutoDisable()<CR>"
  execute "map <silent> <buffer> "
	\ . s:SpellGetOption("spell_next_error_map","<Leader>sn")
	\ . " :call <SID>SpellNextError(1)<CR>"
	"\ . ' /\<\(' . escape(b:spellerrors, "\\") . '\)\><CR>'
  execute "map <silent> <buffer> "
	\ . s:SpellGetOption("spell_previous_error_map","<Leader>sp")
	\ . " :call <SID>SpellNextError(0)<CR>"
endfunction

" Function: s:SpellNextError() {{{2
" search next word with spelling error (way = 1 : next, way = 0 : previous)
function! s:SpellNextError(way) 
  let l:save_hls = &l:hlsearch
  setlocal nohlsearch
  " save position
  let l:cursorPosition = line(".") . " | normal!" . virtcol(".") . " |"
  let l:HLName = ""
  let l:found = -1
  let l:found_col = 0
  let l:first_found = 0
  let l:first_found_col = 0
  " use line and column info to prevent infinite loop
  while(l:found 
	\ && (l:found != l:first_found  || l:first_found_col != l:found_col) 
	\ && l:HLName != "SpellErrors")
    if l:first_found == 0 && l:found != -1
      let l:first_found = l:found
      let l:first_found_col = l:found_col
    endif
    if a:way 
      let l:found = search('\<\(' . b:spellerrors. '\)\>')
    else
      let l:found = search('\<\(' . b:spellerrors. '\)\>', 'b')
    endif
    let l:found_col = col(".")
    let l:HLName = 
      \ synIDattr(synIDtrans(synID(l:found, l:found_col, 1)), "name")
  endwhile
  if l:HLName != "SpellErrors"
    exe l:cursorPosition
  endif
  "echomsg l:found . " " . l:first_found . " ". l:found_col . " " . l:first_found_col . " ".l:HLName
  let &l:hlsearch=l:save_hls
endfunction


" Function: s:SpellSetIskeyword() {{{2
" Set keyword definition, and add language specific keyword. Called when
" setting up a new buffer
function! s:SpellSetIskeyword()
  let l:options="spell_".b:spell_language."_".&filetype."_iskeyword"
  if exists("s:".l:options)
    let l:ik_options=s:SpellGetOption(l:options,s:{l:options})
  else
    let l:ik_options=s:SpellGetOption(l:options,"")
  endif
  "if no filetype and language specific option exists, try to find a language
  "only specific option.
  if l:ik_options == ""
    let l:options="spell_".b:spell_language."_iskeyword"
    if exists("s:".l:options)
      let l:ik_options=s:SpellGetOption(l:options,s:{l:options})
    else
      let l:ik_options=s:SpellGetOption(l:options,"")
    endif
  endif
  if (l:ik_options != "")
    execute "setlocal iskeyword=".l:ik_options
  endif
endfunction



" Function: s:SpellCreateTemp() {{{2
" Create temp file use for fly spell checking. Define various window dependent
" variables.
function! s:SpellCreateTemp()
  if !exists("w:tempname")
    let w:tempname = tempname()
  endif
  let w:wtop=0
  let w:wbottom=0
  let b:my_changedtick=b:changedtick
endfunction

" Function: s:SpellDeleteTemp() {{{2
function! s:SpellDeleteTemp()
  if exists("w:tempname")
    call delete(w:tempname)
  endif
endfunction


" Function: s:SpellCaseAccept() {{{2
" add keyword under cursor to local dictionary, keeping case.
function! s:SpellCaseAccept()
  let @_=s:SpellSystem('(echo "*'.escape(expand("<cword>"),'\"'). '"; echo "#") | '
	\ . b:spell_executable . b:spell_options
	\ . " -a -d ".b:spell_language)
  if exists("b:spellcorrected")
    let b:spellcorrected=b:spellcorrected."\\|".escape(expand("<cword>"),"'\"")
  else
    let  b:spellcorrected=escape(expand("<cword>"),"'\"")
  endif

  syntax case match
  syntax clear SpellCorrected 
  execute "syntax match SpellCorrected \"\\<\\(".b:spellcorrected
	\ . "\\)\\>\" transparent contains=NONE ".b:spell_syntax_options
  if b:spell_auto_jump 
     call s:SpellNextError(1)
  endif
endfunction


" Function: s:SpellAccept() {{{2
" add lowercased keyword under cursor to local dictionary
function! s:SpellAccept()
  let @_=s:SpellSystem('(echo "&'.escape(expand("<cword>"),'\"') . '";echo "#") | '
	\ . b:spell_executable . b:spell_options
	\ . " -a -d ".b:spell_language)
  if exists("b:spellicorrected")
    let b:spellicorrected=b:spellicorrected."\\|".escape(expand("<cword>"),"'\"")
  else
    let  b:spellicorrected=escape(expand("<cword>"),"'\"")
  endif

  syntax case ignore
  syntax clear SpellCorrected 
  execute "syntax match SpellCorrected \"\\<\\(".b:spellicorrected
	\ . "\\)\\>\" transparent contains=NONE ".b:spell_syntax_options
  if b:spell_auto_jump 
     call s:SpellNextError(1)
  endif
endfunction


" Function: s:SpellIgnore() {{{2
" ignore keyword under cursor for current vim session.
function! s:SpellIgnore()
  if exists("b:spellcorrected")
    let b:spellcorrected=b:spellcorrected."\\|".escape(expand("<cword>"),"'\"")
  else
    let  b:spellcorrected=escape(expand("<cword>"),"'\"")
  endif
  syntax case match
  syntax clear SpellCorrected 
  execute "syntax match SpellCorrected \"\\<\\(".b:spellcorrected
	\ . "\\)\\>\" transparent contains=NONE ".b:spell_syntax_options
  if b:spell_auto_jump 
     call s:SpellNextError(1)
  endif
endfunction


" Function: s:SpellCheckLanguage() {{{2
" verify that a language is defined or else defined one.
function! s:SpellCheckLanguage(setisk)
  if !exists("b:spell_syntax_options")
    let b:spell_syntax_options=""
  endif
  if !exists("b:spell_language")
    " take first language
    let b:spell_language=substitute(b:spell_internal_language_list,",.*","","")
    if s:enable_menu
      exec "amenu <silent> disable ".s:menu."Spell.Language.".b:spell_language
    endif
  endif
  if a:setisk
    call s:SpellSetIskeyword()
  endif
endfunction

" Function: s:SpellVerifyLanguage(a:language) {{{2
" Verify the availability of the language for the previously selected
" spell checker.
function! s:SpellVerifyLanguage(language)
  if  b:spell_executable == "ispell" || b:spell_executable == "aspell"
    let l:dirs = 
	  \ system("echo word |". b:spell_executable . b:spell_verify_option 
	    \." -d ". a:language . b:spell_verify_command )
    if v:shell_error != 0
      echo "Language '". a:language ."' not known from ". b:spell_executable ."."
      return 1
    endif
  else
    echo "No driver for '". b:spell_executable."' spell checker."
    return 1
  endif
  return 0
endfunction

" Function: s:SpellGuessLanguage() {{{2
" try to guess spelling language
function! s:SpellGuessLanguage()
  call s:SpellCheckLanguage(1)

  let l:langnum=1
  let l:score=999999
  let l:bestlang=""

  let l:maxlines=s:SpellGetOption("spell_guess_max_lines",0)
  let l:mlang=substitute(b:spell_internal_language_list,",.*","","")

  " better language recognition for mailtype ?
  if (&filetype == "mail")
    let l:spell_options=""
  else
    let l:spell_options=b:spell_options
  endif

  while matchstr(l:mlang,",") == "" 
	\ && l:langnum <= s:SpellGetOption("spell_guess_max_languages",3)
    if (l:maxlines > 0)
      let l:errors=s:SpellSystem("head -" . l:maxlines . " "
	  \. escape(expand("%"),' \')
	  \. " | " . b:spell_executable . l:spell_options . b:spell_verify_option
	  \." -d " . l:mlang . b:spell_verify_command)
    else
      let l:errors=s:SpellSystem(b:spell_executable . l:spell_options 
	    \. b:spell_verify_option . " -d " .  l:mlang . b:spell_verify_command
	    \. " < " . escape(expand("%"),' \'))
    endif
    let l:errors=escape(l:errors,'"')
    let l:index=stridx(l:errors, "\n")
    let l:spellcount=0
    let l:errorcount=0

    "count errors and get the score from it
    while (l:index > 0 && l:errorcount <= l:score)
      " use stridx/strpart instead of sustitute, because it is faster
      let l:errors=strpart(l:errors, l:index+1)
      let l:index=stridx(l:errors, "\n")
      let l:errorcount=l:errorcount+1
    endwhile
    if l:errorcount < l:score
      let l:score = l:errorcount
      let l:bestlang = l:mlang
    endif
    echomsg l:errorcount."-".l:score.":".l:mlang
    let l:langnum = l:langnum + 1

    let l:mlang=substitute(b:spell_internal_language_list,".*\\C" . l:mlang
	  \ . ",\\([^,]\\+\\),.*","\\1","")
  endwhile
  redraw!
  if (l:bestlang != "")
    call s:SpellSetLanguage(l:bestlang)
  endif
endfunction

" Function: s:SpellGetDicoList() {{{2
" try to find a list of installed dictionaries
function! s:SpellGetDicoList()
  let l:default = "english,francais"

  let l:dirfiles=""
  if b:spell_executable == "ispell"
    " Try to get libdir from ispell -vv
    let l:dirs = s:SpellSystem('ispell -vv | '.s:grep.' LIBDIR')
    let l:dirs=substitute(l:dirs,'^.*["]\([^"]*\)["]','\1','')
    " else try some standard installation path (for older ispell ?)
    if !isdirectory(l:dirs)
      if isdirectory("/usr/lib/ispell/")
	let l:dirs =  "/usr/lib/ispell/"
      elseif isdirectory("/usr/local/lib/ispell/")
	let l:dirs =  "/usr/local/lib/ispell/"
      else
	let l:dirs =  "/usr/local/lib"
      endif
    endif

    let l:dirfiles = glob("`".s:find . l:dirs . s:findname . '"*.hash"' . s:findopt ."`")
    let l:dirfiles = substitute(l:dirfiles,"\/[^\n]*\/","","g")
    let l:dirfiles = substitute(l:dirfiles,"[^\n]*-[^\n]*\n","","g")
    let l:dirfiles = substitute(l:dirfiles,"\.hash","","g")
    let l:dirfiles = substitute(l:dirfiles,"\n",",","g")
  elseif b:spell_executable == "aspell"
    " Thanks to Alexandre Beneteau <alexandre.beneteau@wanadoo.fr> for showing
    " me a way to get aspell directory for dictionaries.
    let l:dirs = system('aspell config dict-dir')
    "don't know, why there is a <NUL> char at the end of line ? Get rid of it.
    let l:dirs = substitute(l:dirs,".$","","")

    let l:dirfiles = glob("`".s:find . l:dirs . s:findname . '"*.multi"' . s:findopt ."`")
    let l:dirfiles = substitute(l:dirfiles,"\/[^\n]*\/","","g")
    let l:dirfiles = substitute(l:dirfiles,"[^\n]*-[^\n]*\n","","g")
    let l:dirfiles = substitute(l:dirfiles,"\.multi","","g")
    let l:dirfiles = substitute(l:dirfiles,"\n",",","g")
  endif
  if l:dirfiles != ""
      let l:dirfiles = substitute(l:dirfiles, '[~]', '\\~','')
      return l:dirfiles
  else
      return l:default
  endif
endfunction

" Function: s:SpellGetOption(name, default) {{{2
" Grab a user-specified option to override the default provided.  Options are
" searched in the window, buffer, then global spaces.
function! s:SpellGetOption(name, default)
  if exists("w:" . a:name)
    execute "return w:".a:name
  elseif exists("b:" . a:name)
    execute "return b:".a:name
  elseif exists("g:" . a:name)
    execute "return g:".a:name
  else
    return a:default
  endif
endfunction


"
" Function: s:SpellChangeFileType() {{{2
"
function! s:SpellChangeFileType(ft)
  if !exists("b:spell_syntax_ft") || b:spell_syntax_ft != a:ft
    let b:spell_syntax_ft = a:ft
    call s:SpellTuneCommentSyntax(a:ft)
    unlet! b:spell_buffer_setup
    call s:SpellSetupBuffer()
  endif
endfunction


"
" Function: s:SpellTuneCommentSyntax() {{{2
" Add support to do spell checking inside comment. Idea from engspchk.vim from
" Dr. Charles E. Campbell, Jr. <Charles.Campbell.1@gsfc.nasa.gov>.
" This can be done only for those syntax files' comment blocks that
" contains=@cluster.
function! s:SpellTuneCommentSyntax(ft)
  if !exists("b:spell_syntax_ft") || b:spell_syntax_ft != a:ft
    let b:spell_syntax_ft = a:ft
    " Special treatment for filetype which do not use @Spell cluster.
    if     a:ft == "amiga"
      syn cluster amiCommentGroup		add=SpellErrors,SpellCorrected
      " highlight only in comments (i.e. if SpellErrors are contained).
      let b:spell_syntax_options = "contained"
    elseif a:ft == "bib"
      syn cluster bibVarContents     	contains=SpellErrors,SpellCorrected
      syn cluster bibCommentContents 	contains=SpellErrors,SpellCorrected
      let b:spell_syntax_options = "contained"
    elseif a:ft == "c" || a:ft == "cpp"
      syn cluster cCommentGroup		add=SpellErrors,SpellCorrected
      let b:spell_syntax_options = "contained"
    elseif a:ft == "csh"
      syn cluster cshCommentGroup		add=SpellErrors,SpellCorrected
      let b:spell_syntax_options = "contained"
    elseif a:ft == "dcl"
      syn cluster dclCommentGroup		add=SpellErrors,SpellCorrected
      let b:spell_syntax_options = "contained"
    elseif a:ft == "fortran"
      syn cluster fortranCommentGroup	add=SpellErrors,SpellCorrected
      syn match   fortranGoodWord contained	"^[Cc]\>"
      syn cluster fortranCommentGroup	add=fortranGoodWord
      hi link fortranGoodWord fortranComment
      let b:spell_syntax_options = "contained"
    elseif a:ft == "sh" || a:ft == "ksh" || a:ft == "bash"
      syn cluster shCommentGroup		add=SpellErrors,SpellCorrected
    elseif a:ft == "b"
      syn cluster bCommentGroup		add=SpellErrors,SpellCorrected
      let b:spell_syntax_options = "contained"
    elseif a:ft == "xml"
      syn cluster xmlText		add=SpellErrors,SpellCorrected
      syn cluster xmlString		add=SpellErrors,SpellCorrected
      syn cluster xmlRegionHook	add=SpellErrors,SpellCorrected

      let b:spell_syntax_options = "contained"
    elseif a:ft == "tex"
      syn cluster texCommentGroup		add=SpellErrors,SpellCorrected

      syn cluster texMatchGroup		add=SpellErrors,SpellCorrected

    elseif a:ft == "vim"
      syn cluster vimCommentGroup		add=SpellErrors,SpellCorrected

      let b:spell_syntax_options = "contained"
    elseif a:ft == "otl"
      syn cluster otlGroup		add=SpellErrors,SpellCorrected
      let b:spell_syntax_options = "contained"
    endif

    " @Spell cluster must be defined in new syntax files
    syn cluster Spell                add=SpellErrors,SpellCorrected

    " by default, only errors in Spell cluster are highlight
    if !exists("b:spell_syntax_options")
      let b:spell_syntax_options = "contained"
    endif
    "but not for markup languages, where spelling errors must be also 
    "highlighted outside of the Spell cluster.
    if stridx(s:SpellGetOption("spell_markup_ft",
	  \ ",html,php,xhtml,dtml,tex,mail,text,help,,"), ",".a:ft.",") > -1
      let b:spell_syntax_options = ""
    endif
  endif
endfunction

" Function: s:SpellSetupBuffer() {{{2
" Set buffer dependents variables. This function is called when entering a
" buffer.
function! s:SpellSetupBuffer()
  " If automatic spell checking configuration for each buffer is not enabled,
  " make sure we only do setup for each buffer once:
  if !s:enable_autocommand && exists("b:spell_buffer_setup")
    return
  endif


  " This flag indicates that spell buffer is being, or has been setup.
  " It's put here to avoid possible reentering problem.
  let b:spell_buffer_setup=1

  " If we're editing a directory rather than a file, return now to avoid
  " breaking syntax highlighting. Spell checking directories doesn't look like
  " a necessary feature.
  if isdirectory(expand("%"))
    return
  endif

  let b:spell_shell=s:SpellGetOption("spell_shell","/bin/sh")

  call s:SpellSetSpellchecker(s:SpellGetOption("spell_executable","ispell"))

  let b:spell_filter=s:SpellGetOption("spell_filter","")
  if exists("b:spell_filter") && b:spell_filter != ""
    let b:spell_filter_pipe=b:spell_filter . "|"
  else
    let b:spell_filter_pipe=""
  endif

    " get filetype and speller dependent options.
  let l:options="spell_".b:spell_executable."_".&filetype."_args"
  if exists("s:".l:options)
    let l:ft_options=s:SpellGetOption(l:options,s:{l:options})
  else
    let l:ft_options=s:SpellGetOption(l:options,"")
  endif

  if b:spell_executable == "ispell"
    " -S : sort by probability option.
    let b:spell_options=s:SpellGetOption("spell_options","-S")
  elseif b:spell_executable == "aspell"
    let b:spell_options=s:SpellGetOption("spell_options","")
    "TODO add auto encoding recognition.
  endif
  let b:spell_options = " " . b:spell_options ." " .l:ft_options ." "
  " set on-the-fly spell check, if filetype is set to a type in
  " spell_auto_type variable (particular case for "all" and "none"), and if
  " nothing is known about b:spell_auto_enable (we do not want to re-enable
  " spell checking on a buffer where it was previously disabled.
  let b:spell_auto_type = s:SpellGetOption("spell_auto_type",
      \"tex,mail,text,html,sgml,otl,cvs,none")
  let b:spell_guess_language_ft = s:SpellGetOption("spell_guess_language_ft",
      \"tex,mail,text,html")
  if !exists("b:spell_auto_enable") 
	\ && ( (strlen(&filetype)
		\ && (match(b:spell_auto_type,'\<'.&filetype.'\>') >= 0
		  \ ||match(b:spell_auto_type, '\<all\>') >=0 )
	      \ )
	  \ || (!strlen(&filetype)
		\ && match(b:spell_auto_type, '\<none\>') >=0 )
	  \ )
    " Shall we try to guess the language use ?
    if ( (strlen(&filetype)
	      \ && (match(b:spell_guess_language_ft,'\<'.&filetype.'\>') >= 0
		\ ||match(b:spell_guess_language_ft, '\<all\>') >=0 )
	    \ )
	\ || (!strlen(&filetype)
	      \ && match(b:spell_guess_language_ft, '\<none\>') >=0 )
	\ )
      call s:SpellAutoEnable(1)
    else
      call s:SpellAutoEnable(0)
    endif
  else
    let b:spell_auto_enable=0
    if s:SpellGetOption("spell_insert_mode",1) && exists("b:save_backspace")
      let &backspace=b:save_backspace
    endif
  endif

  call s:SpellCheckLanguage(0)

  call s:SpellTuneCommentSyntax(&l:ft)


  " user need to be able to backspace before the start of insertion if
  " spell_insert_mode is set... (else backspace won't work for the user...)
  if s:SpellGetOption("spell_insert_mode",1)
    let b:save_backspace=&backspace
    if &backspace =~ '[012]'
      set backspace=2
    else
      set backspace-=start backspace+=start
    endif
  endif

  let b:spell_auto_jump = s:SpellGetOption("spell_auto_jump",1)

endfunction

"'''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
" Function: s:SpellInstallDocumentation(full_name, revision)              {{{2
"   Install help documentation.
" Arguments:
"   full_name: Full name of this vim plugin script, including path name.
"   revision:  Revision of the vim script. #version# mark in the document file
"              will be replaced with this string with 'v' prefix.
" Return:
"   1 if new document installed, 0 otherwise.
" Note: Cleaned and generalized by guo-peng Wen
"'''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''

function! s:SpellInstallDocumentation(full_name, revision)
    " Name of the document path based on the system we use:
    if (has("unix"))
        " On UNIX like system, using forward slash:
        let l:slash_char = '/'
        let l:mkdir_cmd  = ':silent !mkdir -p '
    else
        " On M$ system, use backslash. Also mkdir syntax is different.
        " This should only work on W2K and up.
        let l:slash_char = '\'
        let l:mkdir_cmd  = ':silent !mkdir '
    endif

    let l:doc_path = l:slash_char . 'doc'
    let l:doc_home = l:slash_char . '.vim' . l:slash_char . 'doc'

    " Figure out document path based on full name of this script:
    let l:vim_plugin_path = fnamemodify(a:full_name, ':h')
    let l:vim_doc_path    = fnamemodify(a:full_name, ':h:h') . l:doc_path
    if (!(filewritable(l:vim_doc_path) == 2))
        echomsg "Doc path: " . l:vim_doc_path
        execute l:mkdir_cmd . '"' . l:vim_doc_path . '"'
        if (!(filewritable(l:vim_doc_path) == 2))
            " Try a default configuration in user home:
            let l:vim_doc_path = expand("~") . l:doc_home
            if (!(filewritable(l:vim_doc_path) == 2))
                execute l:mkdir_cmd . '"' . l:vim_doc_path . '"'
                if (!(filewritable(l:vim_doc_path) == 2))
                    " Put a warning:
                    echomsg "Unable to open documentation directory"
                    echomsg " type :help add-local-help for more informations."
                    return 0
                endif
            endif
        endif
    endif

    " Exit if we have problem to access the document directory:
    if (!isdirectory(l:vim_plugin_path)
        \ || !isdirectory(l:vim_doc_path)
        \ || filewritable(l:vim_doc_path) != 2)
        return 0
    endif

    " Full name of script and documentation file:
    let l:script_name = fnamemodify(a:full_name, ':t')
    let l:doc_name    = fnamemodify(a:full_name, ':t:r') . '.txt'
    let l:plugin_file = l:vim_plugin_path . l:slash_char . l:script_name
    let l:doc_file    = l:vim_doc_path    . l:slash_char . l:doc_name

    " Bail out if document file is still up to date:
    if (filereadable(l:doc_file)  &&
        \ getftime(l:plugin_file) < getftime(l:doc_file))
        return 0
    endif

    " Prepare window position restoring command:
    if (strlen(@%))
        let l:go_back = 'b ' . bufnr("%")
    else
        let l:go_back = 'enew!'
    endif

    " Create a new buffer & read in the plugin file (me):
    setl nomodeline
    exe 'enew!'
    exe 'r ' . l:plugin_file

    setl modeline
    let l:buf = bufnr("%")
    setl noswapfile modifiable

    norm zR
    norm gg

    " Delete from first line to a line starts with
    " === START_DOC
    1,/^=\{3,}\s\+START_DOC\C/ d

    " Delete from a line starts with
    " === END_DOC
    " to the end of the documents:
    /^=\{3,}\s\+END_DOC\C/,$ d

    " Remove fold marks:
    % s/{\{3}[1-9]/    /

    " Add modeline for help doc: the modeline string is mangled intentionally
    " to avoid it be recognized by VIM:
    call append(line('$'), '')
    call append(line('$'), ' v' . 'im:tw=78:ts=8:ft=help:norl:')

    " Replace revision:
    exe "normal :1s/#version#/ v" . a:revision . "/\<CR>"

    " Save the help document:
    exe 'w! ' . l:doc_file
    exe l:go_back
    exe 'bw ' . l:buf

    " Build help tags:
    exe 'helptags ' . l:vim_doc_path

    return 1
endfunction
"

" Function: s:SpellSystem() {{{2
" Call system function with sh shell.
function! s:SpellSystem(command)
  let l:save_shell = &shell
  let &shell=b:spell_shell
  let l:res = system(a:command)
  let &shell = l:save_shell
  return l:res
endfunction

"Function: s:SpellAddTriggerMapping(key) {{{2
" Add an insert mode mapping triggering line spell checking.
function! s:SpellAddTriggerMapping(key)
  " This function was necessary to prevent the new behavior of vim 6.3 to
  " disable abbreviation in imap. The side effect was that abbreviation were
  " not more available with vimspell åctivated in insert mode.
  let l:oldmap=maparg(a:key,'i')
  if l:oldmap != ""
    silent! iunmap <buffer> <Space>
    silent! iunmap <Space>
  else
    let l:oldmap = "¬"
  endif
  " Using a special char instead of return... Didn't find a mean to use the
  " same code for <CR>...
  if a:key != "÷"
    " Many, many thanks to Hari Krishna Dara who shows me how to map a
    " function call without leaving insert mode !
    echomsg 'imap <buffer><silent> '.a:key.' ' .l:oldmap
	\. '<C-R>='.s:sid.'SpellCheckLine("'.a:key.'")<CR>'
    exec 'imap <buffer> '.a:key.' ' .l:oldmap
	  \. '<C-R>=<SID>SpellCheckLine("'.a:key.'")<CR>'
  else
    echomsg 'imap <buffer><silent> <CR> ' 
	\. '<C-R>='.s:sid.'SpellCheckLine("")<CR><CR>'
    exec 'inoremap <buffer> <CR> ' 
	  \. '<C-R>=<SID>SpellCheckLine("")<CR><CR>'
  end
endfunction

"Function: s:SpellRemoveTriggerMapping(key) {{{1
" Remove an insert mode mapping triggering line spell checking.
function! s:SpellRemoveTriggerMapping(key)
    " Remove spell check from the key mapping,
    " possibly restoring other mappings if present.
    let l:oldmap=maparg("<CR>",'i')
    if a:key != "÷"
      let l:newmap=substitute(l:oldmap,"<C-R>=".s:sid
	    \.'SpellCheckLine("'.a:key.'")<CR>', "", "")
      exec 'silent! iunmap <buffer> '.a:key
      if l:newmap != "" && l:newmap != "¬"
	exec 'inoremap <buffer><silent> '.a:key.' '.l:newmap
      endif
    else
      let l:newmap=substitute(l:oldmap,"<C-R>=".s:sid
	    \.'SpellCheckLine("")<CR><CR>', "", "")
      exec 'silent! iunmap <buffer> <CR>'
      if l:newmap != "" && l:newmap != "¬"
	exec 'inoremap <buffer><silent> <CR> '.l:newmap
      endif
    endif
endfunction

" Section: Spelling functions {{{1

" Function: s:SpellCheck() {{{2
" Spell check the text after *writing* the buffer. Define highlighting and
" mapping for correction and navigation.
function! s:SpellCheck()
  echo "Spell check in progress..."

  " Make sure spelling check environment is correctly setup before we proceed:
  if !exists("b:spell_buffer_setup")
      call s:SpellSetupBuffer()
  endif

  " save position
  let l:cursorPosition = line(".") . "normal!" . virtcol(".") . "|"
  syn case match
  call s:SpellCheckLanguage(1)
  let l:filename=expand("%")
  if strlen(l:filename)
    silent update
  else
    " the buffer is a new one.
    let l:save_mod = &modified
    let l:filename = tempname()
    " save buffer to a temporary file.
    normal 1GO
    if exists("b:spell_filter") && b:spell_filter != ""
      silent execute  ":2,$w!".l:filename
    else
      echomsg ":2,$w ! ". b:spell_filter .' > ' .l:filename
      silent execute  ":2,$w ! ". b:spell_filter .' > ' .l:filename
    endif
    execute "1d"
    let &modified = l:save_mod
  endif
  syn match SpellErrors "xxxxx"
  syn clear SpellErrors

  " This is needed that we can use stridx instead of match for unique error
  " list.
  " Clear b:spellerrors when doing a complete check to get rid of old errors
  let b:spellerrors="nonexisitingwordinthisdociumnt\\"
  let b:spellcorrected="nonexisitingwordinthisdociumnt"
  let b:spellicorrected="nonexisitingwordinthisdociumnt"

  if exists("b:spell_filter") && b:spell_filter != ""
    let l:errors=s:SpellSystem('cat '. escape(l:filename,' \'). 
	  \ "|".b:spell_filter_pipe 
	  \. b:spell_executable . b:spell_options .  b:spell_verify_option ." -d "
	  \. b:spell_language . b:spell_verify_command)
  else
    let l:errors=s:SpellSystem(b:spell_executable . b:spell_options
	  \. b:spell_verify_option . " -d ".b:spell_language
	  \.b:spell_verify_command .  " < ".escape(l:filename,' \'))
  endif

  let l:errors=escape(l:errors,'"')
  let l:index=stridx(l:errors, "\n")
  let l:spellcount=0
  let l:errorcount=0

  while (l:index > 0)
    " use stridx/strpart instead of sustitute, because it is faster
    let l:oneError="|".strpart(l:errors, 0, l:index)."\\"
    let l:errors=strpart(l:errors, l:index+1)
    let l:index=stridx(l:errors, "\n")
    let l:errorcount=l:errorcount+1

    " only add new errors
    " stridx instead of match for better performance
    if(stridx(b:spellerrors, l:oneError) == -1 )
      let b:spellerrors=b:spellerrors . l:oneError
      let l:spellcount=l:spellcount+1
  endif
  endwhile

  " remove unneeded tail
  let b:spellerrors=strpart(b:spellerrors, 0, strlen(b:spellerrors)-1)

  " install regex for higlight
  syntax case ignore
  execute "syntax match SpellCorrected \"\\<\\(".b:spellicorrected
	\ . "\\)\\>\" transparent contains=NONE ".b:spell_syntax_options
  syntax case match
  execute "syntax match SpellErrors \"\\<\\(".b:spellerrors."\\)\\>\" "
	\ . b:spell_syntax_options
  execute "syntax match SpellCorrected \"\\<\\(".b:spellcorrected
	\ . "\\)\\>\" transparent contains=NONE ".b:spell_syntax_options

  call s:SpellContextMapping()
  execute l:cursorPosition
  redraw!
  echo "Spell check done: ".l:spellcount." possible spell errors in "
        \ .l:errorcount." words."
endfunction


" Function: s:SpellCheckWindow() {{{2
" Spell check the text display on the window (+ some lines before and after)
" *without* writing the buffer. Define highlighting and mapping for correction
" and navigation.
function! s:SpellCheckWindow()
  " SpellCreateTemp must have been called and spell_auto_enable must not be
  " set or must be equal to 0
  if !exists("w:wtop") || (exists("b:spell_auto_enable") && b:spell_auto_enable==0)
    return
  endif
  " save position
  let l:cursorPosition = line(".") . "normal!" . virtcol(".") . "|"
  " Make sure spelling check environment is correctly setup before we proceed:
  if !exists("b:spell_buffer_setup")
    call s:SpellSetupBuffer()
  endif
  " get the first and last visible line
  let sosave = &scrolloff
  let &scrolloff = 0
  let l:curpos = line('.')
  norm H
  let wtop = line('.')
  norm L
  let wbottom = line('.')
  if has('jumplist')
    " Purge the jumplist from the two (or less) jump added.
    if wtop != l:curpos
      exec "norm \<C-O>"
    endif
    if wbottom != l:curpos
      exec "norm \<C-O>"
    endif
  else
    norm ``
    norm ``
  endif
  let &scrolloff = sosave

  " has something changed (buffer content or window position) ?
  if wtop == w:wtop && wbottom == w:wbottom && b:my_changedtick == b:changedtick
    execute l:cursorPosition
    return
  endif
  let b:my_changedtick = b:changedtick
  let w:wtop = wtop
  let w:wbottom = wbottom
  let b:save_mod = &modified
  " save portion of buffer we are interested in .
  if exists("b:spell_filter") && b:spell_filter != ""
    silent execute  ":".wtop .",".wbottom."w !". b:spell_filter 
	  \ .' > '.w:tempname
  else
    silent execute  ":".wtop .",".wbottom."w!".w:tempname
  endif
  let &modified = b:save_mod

  " define mappings and syntax highlighting for spelling errors
  syn case match
  call s:SpellCheckLanguage(1)
  syn match SpellErrors "xxxxx"
  syn clear SpellErrors

  " This is needed that we can use stridx instead of match for unique error
  " list
  if exists("b:spellerrors")
    let b:spellerrors=b:spellerrors."\\"
  else
    let b:spellerrors="nonexisitingwordinthisdociumnt\\"
    let b:spellcorrected="nonexisitingwordinthisdociumnt"
    let b:spellicorrected="nonexisitingwordinthisdociumnt"
  endif

  let l:errors=s:SpellSystem(b:spell_executable . b:spell_options
	\. b:spell_verify_option . " -d ".b:spell_language
	\. b:spell_verify_command. " < ".w:tempname)
  let l:errors=escape(l:errors,'"')
  let l:index=stridx(l:errors, "\n")

  while (l:index > 0)
    " use stridx/strpart instead of sustitude, because it is faster
    let l:oneError="|".strpart(l:errors, 0, l:index)."\\"
    let l:errors=strpart(l:errors, l:index+1)
    let l:index=stridx(l:errors, "\n")

    " only add new errors
    " use stridx instead of match for better performance
    if(stridx(b:spellerrors, l:oneError) == -1 )
      let b:spellerrors=b:spellerrors . l:oneError
    endif
  endwhile

  " remove unneeded tail
  let b:spellerrors=strpart(b:spellerrors, 0, strlen(b:spellerrors)-1)

  " install regex for syntax highlighting
  syntax case ignore
  syntax clear SpellCorrected SpellErrors
  execute 'syntax match SpellCorrected "\<\('.b:spellicorrected
	\ . '\)\>" transparent contains=NONE '.b:spell_syntax_options
  syntax case match
  execute 'syntax match SpellErrors "\<\('.b:spellerrors.'\)\>" '
	\ . b:spell_syntax_options
  execute "syntax match SpellCorrected \"\\<\\(".b:spellcorrected
	\ . "\\)\\>\" transparent contains=NONE ".b:spell_syntax_options

  call s:SpellContextMapping()
  execute l:cursorPosition

  "syntax cluster Spell contains=SpellErrors,SpellCorrected
endfunction

" Function: s:SpellCheckLine() {{{2
" Spell check the line under the cursor *without* writing the buffer. Define
" highlighting and mapping for correction and navigation.
" May be called by a mapping on <Space>, for example.
"
function! s:SpellCheckLine(char)

  let l:linepos = line(".") 
  let l:colpos = col(".")
  "echomsg 'exe :.s/\%'.l:colpos.'c./'. a:char . '/'
  if a:char != ""
    exe ':.s/.\%' . l:colpos.'c/'. a:char . '/'
  end
  "echomsg "call cursor(". l:linepos.", ".l:colpos.")"
  call cursor(l:linepos, l:colpos)

  " return if there is not a word character before the cursor
  " or if there is a one character word (use -3 instead of -2, to prevent
  " <C-O> side effect, which move cursor on the last chars. see :h i_CTRL-O
  if  getline(line("."))[col(".") - 3] !~ '\k'
    return ""
  endif

  " Make sure spelling check environment is correctly setup before we proceed:
  if !exists("b:spell_buffer_setup")
    call s:SpellSetupBuffer()
  endif

  " define mappings and syntax highlighting for spelling errors
  syn case match
  call s:SpellCheckLanguage(1)
  syn match SpellErrors "xxxxx"
  syn clear SpellErrors

  " This is needed that we can use stridx instead of match for unique error
  " list
  if exists("b:spellerrors")
    let b:spellerrors=b:spellerrors."\\"
  else
    let b:spellerrors="nonexisitingwordinthisdociumnt\\"
    let b:spellcorrected="nonexisitingwordinthisdociumnt"
    let b:spellicorrected="nonexisitingwordinthisdociumnt"
  endif

  let l:ispexpr = "echo \"".escape(getline('.'),'$[]\"<>`')."\"|".b:spell_filter_pipe
	\ . b:spell_executable . b:spell_options . b:spell_verify_option
	\ . ' -d '.b:spell_language . b:spell_verify_command
  let l:errors=s:SpellSystem(l:ispexpr)
  let l:errors=escape(l:errors,'"')
  let l:index=stridx(l:errors, "\n")

  while (l:index > 0)
    " use stridx/strpart instead of sustitude, because it is faster
    let l:oneError="|".strpart(l:errors, 0, l:index)."\\"
    let l:errors=strpart(l:errors, l:index+1)
    let l:index=stridx(l:errors, "\n")

    " only add new errors
    " use stridx instead of match for better performance
    if(stridx(b:spellerrors, l:oneError) == -1 )
      let b:spellerrors=b:spellerrors . l:oneError
    endif
  endwhile

  " remove unneeded tail
  let b:spellerrors=strpart(b:spellerrors, 0, strlen(b:spellerrors)-1)

  " install regex for syntax highlighting
  syntax case ignore
  syntax clear SpellCorrected SpellErrors
  execute "syntax match SpellCorrected \"\\<\\(".b:spellicorrected
	\ . "\\)\\>\" transparent contains=NONE ".b:spell_syntax_options
  syntax case match
  execute "syntax match SpellErrors \"\\<\\(".b:spellerrors."\\)\\>\" "
	\ . b:spell_syntax_options
  execute "syntax match SpellCorrected \"\\<\\(".b:spellcorrected
	\ . "\\)\\>\" transparent contains=NONE ".b:spell_syntax_options

  call s:SpellContextMapping()
  return ""

  "syntax cluster Spell contains=SpellErrors,SpellCorrected
endfunction


" Function: s:SpellAutoEnable() {{{2
" Enable auto spelling.
function! s:SpellAutoEnable(guess)
  if exists("b:spell_auto_enable") && b:spell_auto_enable
    return
  endif
  if s:SpellGetOption("spell_no_readonly",1) && &readonly
    return
  endif

  " Make sure spelling check environment is correctly setup before we proceed:
  if !exists("b:spell_buffer_setup")
      call s:SpellSetupBuffer()
  endif

  let b:spell_auto_enable = 1
  let filename=bufname(winbufnr(0))
  augroup spellchecker
    execute "autocmd! CursorHold ". filename ." call s:SpellCheckWindow()"
    execute "autocmd! FocusLost ". filename ." call s:SpellCheckWindow()"
    execute "autocmd! BufWinEnter ". filename ." call s:SpellCreateTemp()"
    execute "autocmd! BufEnter ". filename ." call s:SpellCreateTemp()"
    execute "autocmd! BufWinLeave ". filename ." call s:SpellDeleteTemp()"
  augroup END
  call s:SpellCreateTemp()
  if s:enable_menu
    exe "amenu <silent> disable ".s:menu."Spell.Auto"
    exe "amenu <silent> enable ".s:menu."Spell.No\\ auto"
  endif
  " Add <space> mapping if the auto spell is enabled and required in insert
  " mode (by mean of <space> mapping).
  " An existing mapping is responsible to insert a space, if it's there.
  if s:SpellGetOption("spell_insert_mode",1) 
	\ && exists("b:spell_auto_enable") && b:spell_auto_enable
    call s:SpellAddTriggerMapping("<Space>")
    call s:SpellAddTriggerMapping("÷")
    call s:SpellAddTriggerMapping('.')
    call s:SpellAddTriggerMapping(',')
"    if maparg('<CR>','i') == ""
"      exec 'inoremap <buffer><silent> <CR> <C-R>=<SID>SpellCheckLine("\n")<CR><CR>'
"    endif
  endif
  if a:guess
    call s:SpellGuessLanguage()
  endif
endfunction

" Function: s:SpellAutoDisable() {{{2
" Disable auto spelling
function! s:SpellAutoDisable()
  call s:SpellExit()
  if !exists("b:spell_auto_enable") || b:spell_auto_enable == 0
    return
  endif

  " Make sure spelling check environment is correctly setup before we proceed:
  if !exists("b:spell_buffer_setup")
      call s:SpellSetupBuffer()
  endif

  let b:spell_auto_enable = 0
  augroup spellchecker
    silent "autocmd! CursorHold ". filename
    silent "autocmd! FocusLost ". filename
    silent "autocmd! BufWinEnter ". filename
    silent "autocmd! BufWinLeave ". filename
  augroup END
  let  &updatetime=g:spell_old_update_time
  unlet! w:wtop
  if s:enable_menu
    exe "amenu <silent> enable ".s:menu."Spell.Auto"
    exe "amenu <silent> disable ".s:menu."Spell.No\\ auto"
  endif
  if s:SpellGetOption("spell_insert_mode",1)

    call s:SpellRemoveTriggerMapping("<Space>")
    call s:SpellRemoveTriggerMapping("÷")
    call s:SpellRemoveTriggerMapping('.')
    call s:SpellRemoveTriggerMapping(',')
  endif
endfunction

" Function: s:SpellSetSpellchecker(a:spellchecker) {{{2
" Select a spell checker executable
function! s:SpellSetSpellchecker(prog)
  if matchstr(s:known_spellchecker,a:prog) == ""
    echo "No driver for '".a:prog."' spell checker."
    return
  endif
  " Nothing to do if the executable is the same as the one previously set.
  if exists("b:spell_executable") && b:spell_executable == a:prog
    return
  endif

  " Make sure spelling check environment is correctly setup before we proceed:
  if !exists("b:spell_buffer_setup")
      call s:SpellSetupBuffer()
  endif

  if s:enable_menu && exists("b:spell_executable")
    exec "amenu <silent> enable ".s:menu."Spell.".b:spell_executable
  endif
  let b:spell_executable=a:prog
  if (b:spell_executable == "ispell")
    let b:spell_verify_command = " "
    let b:spell_verify_option = " -l "
  else
    let b:spell_verify_command = " list "
    let b:spell_verify_option = " "
  endif
  if s:enable_menu
    exec "amenu <silent> disable ".s:menu."Spell.".b:spell_executable
  endif
  let b:spell_internal_language_list=s:SpellGetOption("spell_language_list", "")
  if  b:spell_internal_language_list != ""
    " verify user defined language list
    let l:must_verify = 1
  else
    "get language list (spell checker dependent. Ought to be valid)
    let b:spell_internal_language_list=s:SpellGetDicoList()
    let l:must_verify = 0
  endif
  let b:spell_internal_language_list = b:spell_internal_language_list.","

   " Init language menu
  if s:enable_menu
    exec "aunmenu <silent> ".s:menu."Spell.Language"
  endif
  let l:mlang=substitute(b:spell_internal_language_list,",.*","","")
  while matchstr(l:mlang,",") == ""
    if s:enable_menu && (!l:must_verify || !s:SpellVerifyLanguage(l:mlang))
      exec "amenu <silent> ".s:prio."50 ".s:menu."Spell.&Language.".l:mlang
	    \ . "  :SpellSetLanguage ".l:mlang."<CR>"
    endif
    " take next one
    let l:mlang=substitute(b:spell_internal_language_list,".*\\C" . l:mlang
	  \ . ",\\([^,]\\+\\),.*","\\1","")
  endwhile
  "force spell check
  let b:my_changedtick=0
  " remove actual highlighting and spelling errors
  unlet! b:spellerrors
  syn match SpellErrors "xxxxx"
  syn match SpellCorrected "xxxxx"
  syn clear SpellErrors
  syn clear SpellCorrected
endfunction

" Function: s:SpellChangeLanguage() {{{2
" Select next available language
function! s:SpellChangeLanguage()
  " Make sure spelling check environment is correctly setup before we proceed:
  if !exists("b:spell_buffer_setup")
      call s:SpellSetupBuffer()
  endif

  if !exists("b:spell_language")
    " take first language
    let b:spell_language=substitute(b:spell_internal_language_list,",.*","","")
  else
    if s:enable_menu
      exec "amenu <silent> enable ".s:menu."Spell.Language.".b:spell_language
    endif
    " take next one
    let l:res=substitute(b:spell_internal_language_list,".*\\C"
	  \ . b:spell_language . ",\\([^,]\\+\\),.*","\\1","")
    if matchstr(l:res,",") != ""
      " if no next, take the first
      let l:res=substitute(b:spell_internal_language_list,",.*","","")
    endif
    let b:spell_language=l:res
  endif
  if s:enable_menu
    exec "amenu <silent> disable ".s:menu."Spell.Language.".b:spell_language
  endif
  " force spell check
  let b:my_changedtick=0
  " remove actual highlighting and spelling errors
  unlet! b:spellerrors
  syn match SpellErrors "xxxxx"
  syn match SpellCorrected "xxxxx"
  syn clear SpellErrors
  syn clear SpellCorrected
  call s:SpellSetIskeyword()
  echo "Language: ".b:spell_language
endfunction

" Function: s:SpellSetLanguage(a:language) {{{2
" Select a language
function! s:SpellSetLanguage(language)
  " Make sure spelling check environment is correctly setup before we proceed:
  if !exists("b:spell_buffer_setup")
      call s:SpellSetupBuffer()
  endif

  call s:SpellCheckLanguage(1)
  if s:SpellVerifyLanguage(a:language)
    return
  endif

  "create menu if a new language was requested.
  if match(b:spell_internal_language_list, '\<'.a:language.'\>') < 0
    exec "amenu <silent> ".s:prio."50 ".s:menu."Spell.&Language.".a:language
	  \ . "  :SpellSetLanguage ".a:language."<CR>"
  endif

  if s:enable_menu
    exec "amenu <silent> enable ".s:menu."Spell.Language.".b:spell_language
  endif
  let b:spell_language=a:language
  if s:enable_menu
    exec "amenu <silent> disable ".s:menu."Spell.Language.".b:spell_language
  endif
  "force spell check
  let b:my_changedtick=0
  " remove actual highlighting and spelling errors
  unlet! b:spellerrors
  syn match SpellErrors "xxxxx"
  syn match SpellCorrected "xxxxx"
  syn clear SpellErrors
  syn clear SpellCorrected
  echo "Language: ".b:spell_language
endfunction

" Function: s:SpellExit() {{{2
" remove syntax highlighting and mapping defined for spell checking.
function! s:SpellExit()
  silent! "unmap <silent> <buffer> "
	\ . s:SpellGetOption("spell_case_accept_map","<Leader>si")
  silent "unmap <silent> <buffer> "
	\ . s:SpellGetOption("spell_accept_map","<Leader>su")
  silent "unmap <silent> <buffer> "
	\ . s:SpellGetOption("spell_ignore_map","<Leader>sa")
  silent "unmap <silent> <buffer> "
	\ . s:SpellGetOption("spell_next_error_map","<Leader>sn")
  silent "unmap <silent> <buffer> "
	\ . s:SpellGetOption("spell_previous_error_map","<Leader>sp")
  silent "unmap <silent> <buffer> "
	\ . s:SpellGetOption("spell_exit_map","<Leader>sq")
  syn match SpellErrors "xxxxx"
  syn match SpellCorrected "xxxxx"
  syn clear SpellErrors
  syn clear SpellCorrected
endfunction

" Section: Command definitions {{{1
com! SpellAutoEnable call s:SpellAutoEnable(1)
com! SpellAutoDisable call s:SpellAutoDisable()
com! SpellCheck call s:SpellCheck()
com! SpellExit call s:SpellExit()
com! SpellProposeAlternatives call s:SpellProposeAlternatives()
com! SpellMenuAlternatives call s:SpellMenuAlternatives()
com! SpellChangeLanguage call s:SpellChangeLanguage()
com! SpellCheckLine call s:SpellCheckLine()
com! -nargs=1 SpellSetLanguage call s:SpellSetLanguage(<f-args>)
com! -nargs=1 SpellSetSpellchecker call s:SpellSetSpellchecker(<f-args>)
      \<Bar> echo "Spell checker: ".<f-args>

" Allow reloading vimspell.vim
com! SpellReload unlet! loaded_vimspell | SpellAutoDisable
      \ | runtime plugin/vimspell.vim

" Section: Plugin  mappings {{{1
nnoremap <silent> <unique> <Plug>SpellCheck          :SpellCheck<CR>
nnoremap <silent> <unique> <Plug>SpellCheckLine      :SpellCheckLine<CR>
nnoremap <silent> <unique> <Plug>SpellExit           :SpellExit<CR>
nnoremap <silent> <unique> <Plug>SpellChangeLanguage :SpellChangeLanguage<CR>
nnoremap <silent> <unique> <Plug>SpellAutoEnable     :SpellAutoEnable<CR>
nnoremap <silent> <unique> <Plug>SpellAutoDisable    :SpellAutoDisable<CR>
nnoremap <silent> <unique> <Plug>SpellProposeAlternatives
      \ :SpellProposeAlternatives<CR>
nnoremap <silent> <unique> <Plug>SpellMenuAlternatives
      \ :SpellMenuAlternatives<CR>

" Section: Default mappings {{{1
if !hasmapto('<Plug>SpellCheck')
  nmap <silent> <unique> <Leader>ss <Plug>SpellCheck
endif


if !hasmapto('<Plug>SpellAutoEnable')
  nmap <silent> <unique> <Leader>sA <Plug>SpellAutoEnable
endif

if !hasmapto('<Plug>SpellProposeAlternatives')
  nmap <silent> <unique> <Leader>s? <Plug>SpellProposeAlternatives
endif

if !hasmapto('<Plug>SpellChangeLanguage')
  nmap <silent> <unique> <Leader>sl <Plug>SpellChangeLanguage
endif

if &mousemodel =~ 'popup'
  amenu <silent> 1.8 PopUp.-SEPx- <nop>
  nnoremap <silent> <RightMouse> <LeftMouse>:SpellMenuAlternatives<CR><RightMouse>
endif


" Section: Menu items {{{1
"
" The menu will be disable if the root menu name is "-". The s:enable_menu
" flag indicates whether vimspell menu is enabled or not.
let s:menu=s:SpellGetOption("spell_root_menu","Plugin.")
let s:enable_menu=(s:menu ==# '-') ? 0 : 1
if s:enable_menu
  let s:prio=s:SpellGetOption("spell_root_menu_priority","500.")
      \ . s:SpellGetOption("spell_menu_priority","10.")
  if exists("mapleader") && mapleader != ""
    let s:ml=mapleader
  else
    let s:ml="\\"
  endif
  let s:sq=substitute(s:SpellGetOption("spell_exit_map","<Leader>sq"), '<Leader>', s:ml, "") 
  let s:sp=substitute(s:SpellGetOption("spell_previous_error_map","<Leader>sp"), '<Leader>', s:ml, "") 
  let s:sn=substitute(s:SpellGetOption("spell_next_error_map","<Leader>sn"), '<Leader>', s:ml, "") 
  let s:sa=substitute(s:SpellGetOption("spell_ignore_map","<Leader>sa"), '<Leader>', s:ml, "") 
  let s:su=substitute(s:SpellGetOption("spell_accept_map","<Leader>su"), '<Leader>', s:ml, "") 
  let s:si=substitute(s:SpellGetOption("spell_case_accept_map","<Leader>si"), '<Leader>', s:ml, "") 

  exe "amenu <silent> ".s:prio."10  ".s:menu
	\ . "Spell.&Spell       <Plug>SpellCheck"
  exe "amenu <silent> ".s:prio."15  ".s:menu
	\ . "Spell.&Off         <Plug>SpellExit"
  exe "amenu <silent> ".s:prio."30  ".s:menu
	\ . "Spell.&Language.Next\ one <Plug>SpellChangeLanguage"
  exe "amenu <silent> ".s:prio."    ".s:menu
	\ . "Spell.&Language.-Sep-   :"
  exe "amenu <silent> ".s:prio."40  ".s:menu
	\ . "Spell.-Sep-		:"
  exe "amenu <silent> ".s:prio."45  ".s:menu
	\ . "Spell.A&uto <Plug>SpellAutoEnable"
  exe "amenu <silent> ".s:prio."50  ".s:menu
	\ . "Spell.&No\\ auto<tab>".s:sq."  <Plug>SpellAutoDisable  "
  exe "amenu <silent> ".s:prio."100 ".s:menu
	\ . "Spell.-Sep2-	    :"
  exe "amenu <silent> ".s:prio."101 ".s:menu
	\ . "Spell.aspell :SpellSetSpellchecker aspell<CR>"
  exe "amenu <silent> ".s:prio."102 ".s:menu
	\ . "Spell.ispell :SpellSetSpellchecker ispell<CR>"
  exe "amenu <silent> disable ".s:menu."Spell.No\\ auto"
  exe "amenu <silent> ".s:prio."200 ".s:menu
        \ . "Spell.-Sep3-                           :"
  exe "amenu <silent> ".s:prio."210 ".s:menu
        \ . "Spell.&Previous\\ error<tab>".s:sp."                :call <SID>SpellNextError(0)<CR>"
  exe "amenu <silent> ".s:prio."220 ".s:menu
        \ . "Spell.N&ext\\ error<tab>".s:sn."                    :call <SID>SpellNextError(1)<CR>"
  exe "amenu <silent> ".s:prio."230 ".s:menu
        \ . "Spell.&Ignore<tab>".s:sa."                          :call <SID>SpellIgnore()<CR>"
  exe "amenu <silent> ".s:prio."235  ".s:menu
        \ . "Spell.&Alternative <Plug>SpellProposeAlternatives"
  exe "amenu <silent> ".s:prio."240 ".s:menu
        \ . "Spell.Insert\\ into\\ &dict\\.<tab>".s:si."         :call <SID>SpellCaseAccept()<CR>"
  exe "amenu <silent> ".s:prio."250 ".s:menu
        \ . "Spell.Insert\\ into\\ di&ct\\.\\ (lc)<tab>".s:su."  :call <SID>SpellAccept()<CR>"
endif


" Section: Doc installation {{{1
"
  let s:revision=
	\ substitute("$Revision: 1.95 $",'\$\S*: \([.0-9]\+\) \$','\1','')
  silent! let s:install_status =
      \ s:SpellInstallDocumentation(expand('<sfile>:p'), s:revision)
  if (s:install_status == 1)
      echom expand("<sfile>:t:r") . ' v' . s:revision .
		\ ': Help-documentation installed.'
  endif


" Section: Plugin init {{{1
"
  highlight default SpellErrors ctermfg=Red guifg=Red cterm=underline gui=underline term=reverse
    " empty augroup spellchecker
  augroup spellchecker
    au!
  augroup END
  let g:spell_old_update_time=&updatetime
  let &updatetime=s:SpellGetOption("spell_update_time",2000)

  " If the menu and the automatic spelling check are both disabled, we should
  " not install BufEnter autocommand to minimize performance impact. The flag
  " s:enable_autocommand is used to indicate whether we should enable
  " automatic spell checking configuration for each buffer:
  let s:enable_autocommand=(s:enable_menu  ||
        \ s:SpellGetOption("spell_auto_type", "Default") != "")

  augroup SpellCommandPlugin
    au!
  augroup END

  if s:enable_autocommand
    augroup SpellCommandPlugin
      au BufEnter * call s:SpellSetupBuffer()
      au FileType * call s:SpellChangeFileType(expand("<amatch>"))
    augroup END
  endif


" Section: Plugin completion {{{1
let loaded_vimspell=2
let &cpo = s:cpo_save
"}}}1
finish

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Section: Documentation content                                          {{{1
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
=== START_DOC
*vimspell.txt*   On the fly spell checker with ispell/aspell.        #version#


                        VIMSPELL REFERENCE MANUAL~


Spelling text with the plugin "Vimspell"


==============================================================================
CONTENT                                                    *vimspell-contents* 

    Installation        : |vimspell-install|
    Vimspell intro      : |vimspell|
    Requirements        : |vimspell-requirements|
    Vimspell commands   : |vimspell-commands|
    Customization       : |vimspell-customize|
    Bugs                : |vimspell-bugs|
    Faq                 : |vimspell-tips|
    Todo list           : |vimspell-todo|

==============================================================================
1. vimspell Installation {{{2                               *vimspell-install*

    In order to install the plugin, place the vimspell.vim file into a plugin
    directory in your runtime path (please see |add-global-plugin| and
    |'runtimepath'|).

    By default, on-the-fly spell checking is enable for certain filetypes
    only. To disable it put the following line in your .vimrc : >
 	  let spell_auto_type = ""
<
    In order to activate it for a filetype, you should redefine the
    |spell_auto_type| variable (see below).

    |vimspell| may be customized by setting variables, creating maps, and
    specifying event handlers.  Please see |vimspell-customize| for more
    details.

    Set 'mousemodel' to "popup_setpos" in order to enable popup menus (see
    |vimspell-popup|).

                                                          *vimspell-auto-help*
    This help file is automagically generated when the |vimspell| script is
    loaded for the first time.

==============================================================================
1.1. vimspell requirements                             *vimspell-requirements*

    Vimspell needs the following external tools :
     - 'ispell' or 'aspell' spell checkers

    It has been tested with vim 6.1, but should also work with vim 6.0.

==============================================================================
2. vimspell intro {{{2                                              *vimspell*
                                                      *spell* *vimspell-intro*

   vimspell script provides functions and mappings to check spelling ; either
   on demand on the whole buffer, or for the current visible window whenever
   the cursor is idle for a certain time.

   The default mappings are defined as follow (By default, <Leader> stands
   for '\'. See |Leader| for more info) :

   <Leader>ss   - write file, spellcheck file & highlight spelling mistakes.
   <Leader>sA   - start autospell mode.
   <Leader>sq   - return to normal syntax coloring and disable auto spell
	  	  checking.
   <Leader>sl   - switch between languages.
   <Leader>sn   - go to next error.
   <Leader>sp   - go to previous error.
   <Leader>si   - insert word under cursor into user dictionary.
   <Leader>su   - insert word under cursor as lowercase into user dictionary.
   <Leader>sa   - accept word for this session only.
   <Leader>s?   - check for alternatives.
   <RightMouse> - open a popup menu with alternatives (see |vimspell-popup|).

   See |vimspell-mappings-override| and |vimspell-options| to learn how to
   override those default mappings.

==============================================================================
3. vimspell commands	{{{2                               *vimspell-commands*

    See |vimspell-intro| for default mapping. Vimspell defines the following
    commands:

    :SpellCheck                                                  *:SpellCheck*
      Spell check the text after _writing_ the buffer. Define highlighting and
      mapping for correction and navigation.

    :SpellAutoEnable                                        *:SpellAutoEnable*
      Enable on-the-fly spell checking.

    :SpellAutoDisable                                      *:SpellAutoDisable*
      Disable on-the-fly spell checking.

    :SpellChangeLanguage                                *:SpellChangeLanguage*
      Select the next language available.

    :SpellSetLanguage                                      *:SpellSetLanguage*
      Set the language to the one given as a parameter.

    :SpellSetSpellchecker                              *:SpellSetSpellchecker*
      Set the spell checker to the string given as a parameter (currently,
      aspell or ispell are supported).

    :SpellProposeAlternatives                      *:SpellProposeAlternatives*
      Propose alternative for keyword under cursor. Define mapping used to
      correct the word under the cursor.

    :SpellExit                                                    *:SpellExit*
      Remove syntax highlighting and mapping defined for spell checking.

    :SpellReload                                                *:SpellReload*
      Reload vimspell script.

==============================================================================
4. Vimspell customization  {{{2                           *vimspell-customize*

4.1. General configuration {{{3 ~
--------------------------
                                          *loaded_vimspell* *vimspell-disable*
    You can disable this script by putting the following line in your |vimrc| >
      let loaded_vimspell = 1
<

    You can define your own color scheme for error highlighting, by setting
    |highlight| on SpellErrors group. For example: >
      highlight SpellErrors  guibg=Red guifg=Black
<
    If no words appear to be highlighted after a spell check, try to put the
    following lines in your |vimrc|: >
      highlight SpellErrors ctermfg=Red guifg=Red
 	   \ cterm=underline gui=underline term=reverse
<
							      *vimspell-popup*
    You can right click on a word to get a popup menu which contains a "Spell"
    sub-menu with all alternatives for the word under the cursor.
    In order to get the popup menu, you must be sure that 'mousemodel' is set
    to "popup" or to "popup_setpos" (see |'mousemodel'|).

4.2. Mapping documentation: {{{3 ~
---------------------------
                                                  *vimspell-mappings-override*
    By default, a global mapping is defined for some commands.  User-provided
    mappings can be used instead by mapping to <Plug>CommandName. This is
    especially useful when these mappings collide with other existing mappings
    (vim will warn of this during plugin initialization, but will not clobber
    the existing mappings).

    For instance, to override the default mapping for :SpellCheck to set it to
    \sc, add the following to the |vimrc|:
>
      nmap \sc <Plug>SpellCheck
<
    The default global mappings are as follow:

        <Leader>ss  SpellCheck
        <Leader>sA  SpellAutoEnable
        <Leader>s?  SpellProposeAlternatives
        <Leader>sl  SpellChangeLanguage

    Other mapping are defined according to the context of utilisation, and can
    be redefined by mean of buffer-wise variables. See |vimspell-options| here
    after.

4.3. Options documentation: {{{3 ~
---------------------------
                                                            *vimspell-options*
    Several variables are checked by the script to customize vimspell
    behavior. Note that variables are looked for in the following order :
    window dependent variables first, buffer dependent variables next and
    global ones last (See |internal-variables|, |buffer-variable|,
    |window-variable| and |global-variable|).

    spell_case_accept_map                              *spell_case_accept_map*
      This variable, if set, determines the mapping used to accept the word
      under the cursor, taking case into account. Defaults to: >
 	  let spell_case_accept_map = "<Leader>si"
<
      When using 'ispell' the accepted words are put in the
      ./.ispell_<language> file if it exists or in the
      $HOME/.ispell_<language> file.


    spell_accept_map                                        *spell_accept_map*
      This variable, if set, determines the mapping used to accept a lowercase
      version of the word under the cursor. Defaults to: >
 	  let spell_accept_map = "<Leader>su"
<

    spell_ignore_map                                        *spell_ignore_map*
      This variable, if set, determines the mapping used to ignore the
      spelling error for the current session. Defaults to: >
 	  let spell_ignore_map = "<Leader>sa"
<

    spell_next_error_map                                *spell_next_error_map*
      This variable, if set, determines the mapping used to jump to the next
      spelling error. Defaults to: >
 	  let spell_next_error_map = "<Leader>sn"
<

    spell_previous_error_map                        *spell_previous_error_map*
      This variable, if set, determines the mapping used to jump to the
      previous spelling error. Defaults to: >
 	  let spell_previous_error_map = "<Leader>sp"
<

    spell_exit_map                                            *spell_exit_map*
      This variable, if set, determines the mapping used to exit from
      spelling-checker mode. Defaults to: >
 	  let spell_exit_map = "<Leader>sq"
<

    spell_executable                                        *spell_executable*
      This variable, if set, defines the name of the spell-checker. Defaults
      to: >
 	  let spell_executable = "ispell"
<

    spell_filter                                                *spell_filter*
      This variable, if set, defines the name of a script designed to filter
      out certain words from the standard input to the standard output.
      Defaults to: >
 	  let spell_filter = ""
<     For example : >
	  let spell_filter="grep -v '^#' "
<     would prevent line beginning by # to be spell checked.


    spell_update_time                                      *spell_update_time*
      This variable, if set, defines the duration (in ms) between the last
      cursor movement and the on-the-fly spell check. Defaults to: >
 	  let spell_update_time = 2000
<

    spell_language_list      *vimspell_default_language* *spell_language_list*
      This variable, if set, defines the languages available for spelling. The
      language names are the ones passed as an option to the spell checker.
      Defaults to the languages for which a dictionary is present, or if none
      can be found in the standard location, to: >
 	  let spell_language_list = "english,francais"
<     Note: The first language of this list is the one selected by default.
      An empty list force vimspell to select dictionaries installed for the
      selected spell checker.


    spell_options                                              *spell_options*
      This variable, if set, defines additional options passed to the spell
      checker executable. Defaults for ispell to: >
 	  let spell_options = "-S"
<     and for aspell to: >
 	  let spell_options = ""
<

    spell_auto_type                                          *spell_auto_type*
      This variable, if set, defines a list of filetype for which spell check
      is done on the fly by default. Set it to "all" if you want on-the-fly
      spell check for every filetype. You can use the token "none" if you
      want on-the-fly spell check for files which do not have a filetype.
      Defaults to: >
 	  let spell_auto_type = "tex,mail,text,html,sgml,otl,cvs,none"
<     Note: the "text" and "otl" filetypes are not defined by vim. Look at
      |new-filetype| to see how you could yourself define a new filetype.


    spell_guess_language_ft                          *spell_guess_language_ft*
      This variable, if set, defines a list of filetype for which vimspell try 
      to automatically find which language is in use.  Set it to "all" if you 
      want to use the guess heuristic for every filetype. You can use the 
      token "none" if you want to use it for files which do not have a 
      filetype.
      Defaults to: >
	  let spell_guess_language_ft = "tex,mail,text,html"
<     Note: the "text" filetype is not defined by vim. Look at |new-filetype| 
      to see how you could yourself define a new filetype.
      
    
    spell_guess_max_languages                      *spell_guess_max_languages*
      This variable if set, set up how many languages are checked against the 
      current buffer when vimspell try to determines the language in use. At 
      most 'spell_ispell_sgml_args' are checked, and the one which best 
      matches the current buffer is selected.
      Defaults to: >
	  let spell_guess_max_languages = 3
<


    spell_guess_max_lines                              *spell_guess_max_lines*
      This variable, if set to a positive number, defines the number of lines
      considered when guessing the language of the buffer. Otherwise, the
      entire buffer is considered. Note that this feature makes use of the
      'head' unix command.
      Defaults to: >
 	  let spell_guess_max_lines = 0
<

    spell_insert_mode                                      *spell_insert_mode*
      This variable if set, set up a hack to allow spell checking in insert
      mode. This is normally not possible by mean of autocommands, but is
      done with a map to the <Space> key. Each time that <Space> is hit,
      the current line is spell checked. This feature can slow down vim
      enough but is otherwise very nice.
      Note that the mapping is defined only when spell check is done on the
      fly (see |spell_auto_type|). The 'backspace' may be modified when this
      option is set.
      Defaults to: >
 	  let spell_insert_mode = 1
<

    spell_no_readonly                                      *spell_no_readonly*
      This variable, if set, defines if read-only files are spell checked or
      not. Defaults to: >
 	  let spell_no_readonly = 1  "no spell check for read only files.
<

    spell_auto_jump		                             *spell_auto_jump*
      This variable, if set, make the cursor automatically jump to the next 
      error after: 
       - inserting a word in the user dictionary, 
       - accepting a word for the editing session,
       - accepting a suggestion from the alternative list.
      Defaults to: >
	  let spell_auto_jump = 1
<

    spell_markup_ft                                          *spell_markup_ft*
      This variable, if set, defines a list of filetype for which vimspell
      highlights all errors, and not only those included in the @Spell cluster 
      (see |syn-cluster|).
      Defaults to: >
	  let spell_markup_ft = ",html,php,xhtml,dtml,tex,mail,help,text,,"
<     Note the beginning and ending commas.


    spell_{spellchecker}_{filetype}_args
                               *spell_spellchecker_args* *spell_filetype_args*
      Those variables, if set, define the options passed to the "spellchecker"
      executable for the files of type "filetype". By default, they are set
      to options known by ispell and aspell for tex, html, sgml, email
      filetype. See also |vimspell-ispell-dont-work| below.
      For example: >
 	  let spell_aspell_tex_args = "-t"
<

    spell_{language}_iskeyword                               *spell_iskeyword*
      Those variables if set define the characters which are part of a word
      in the selected language. See |iskeyword| for more informations. 
      Note that the hereafter described variables (the filetype and language
      dependent ones) overrides, if exists, the spell_{language}_iskeyword
      variables.
      The following ones are defined: >
	   let s:spell_francais_iskeyword  = "@,48-57,_,192-255,39,½,æ,¼,Æ,-"
	   let s:spell_russian_iskeyword = "@,48-57,_,192-255,^a-z,^A-Z" 
	   let s:spell_english_iskeyword = "@,48-57,_,192-255,^192-255"
<     which says for example that the quote, the hyphen and some other digraphs
      must be considered as being part of french words.


    spell_{language}_{filetype}_iskeyword 
      Those variable have exactly the same impact as the previous one, except
      that they are filetype dependant. 
      The following is defined: >
	   let s:spell_german_tex_iskeyword  = "@,48-57,192-255,=,\,34"
<     which says that the double quote (char 34) must be considered as being
      part of words in german tex files.


    spell_root_menu                                          *spell_root_menu*
      This variable, if set, give the name of the menu in which the vimspell
      menu will be put. If set to '-', no menu are displayed. Defaults to: >
 	  let spell_root_menu = "Plugin."
<     Note the termination dot.
      spell_root_menu_priority must be set accordingly. Set them both to "" if
      you want vimspell menu in the main menu bar.


    spell_root_menu_priority                        *spell_root_menu_priority*
      This variable, if set, give the priority of the menu containing the
      vimspell menu. Defaults to: >
 	  let spell_root_menu_priority = "500."
<     which is quite on the right of the menu bar.
      Note the termination dot.


    spell_menu_priority                                  *spell_menu_priority*
      This variable, if set, give the priority of the vimspell menu. Defaults
      to: >
 	  let spell_menu_priority = "10."
<     Note the termination dot.


    spell_shell                                                  *spell_shell*
      This variable, if set, gives the name of the shell use for the system
      calls. Defaults to: >
 	  let spell_menu_priority = "/bin/sh"
<     Note that other shells may not work as expected. For example, tcsh
      quoting differs from bash.


==============================================================================
5. Vimspell FAQ  {{{2                                           *vimspell-faq*
                                                   *vimspell-ispell-dont-work*
   When I try to spell check an HTML file using ispell, I got an error like
   "Not an editor command:  -pfile | -wchars | ..."

	By default, vimspell pass the "-H" option to tell ispell that the file
	in which he is looking for errors is an HTML file. This option changed
	accross ispell versions. You should adjust the
	'spell_ispell_html_args' and 'spell_ispell_sgml_args' variables
	appropriately. For example, with ispell 3.1.20, you should set the
	following lines in your .vimrc : >
	  let spell_ispell_html_args  = "-h"
	  let spell_ispell_sgml_args  = "-h"
<
                                                 *vimspell-latex-geman-quotes*
   Use of vimspell for latex file in german.

	Vimspell should work fine with german and latex but one problem stays:
	the german quotation marks which can be written "` and "' in latex.
	When replacing a word between german quotation marks, the last double
	quote will be replaced, destroying the quotation marks.
	The solution is to use \glqq and \grqq for german left and right
	quotation marks.

   How to minimize overhead on a small configuration.

       Do not use menu nor auto spell. Put the following line in your .vimrc: >
         let spell_root_menu   = '-'
         let spell_auto_type   = ''
         let spell_insert_mode = 0
<  

       If you use the automatic guess language feature, you can also limit the
       number of lines used for guessing (this feature need the external
       "head" command to work) : >
         let spell_guess_max_lines = 30
<
       Or you can disable this feature by putting the following line in your
       .vimrc: >
	 let spell_guess_language_ft = ""
<

   What does this "warning : vimspell doesn't work with csh shells (:help
   'shell')" message mean ?

       It means that [t]csh quoting of double quote is bit strange, as the
       string "aaaa\"aa" produces an 'unmatch "' error. Instead of writing a
       different escape function for each shell on the world, I've decided to
       force the shell to /bin/sh. 
       You can disable the warning by putting the following line in your
       .vimrc: >
         set shell=/bin/sh
<
   The number of errors returned by vimspell doesn't match the number of 
   keywords underlined !
       
       Yes. It is a known problem. When calling the SpellCheck function, 
       [ai]spell is fed with the whole text, even if some parts are not 
       concerned by error highlighting (for exemple, in shell scripts, only 
       errors in strings and comments are highlighted).

   Aspell doesn't understand the "oe" ligature, in iso-8859-15 encoding.

       Aspell does understant the ligature, but you must use the good 
       dictionary. You can get one at : >
	  http://geodisi.u-strasbg.fr/~daurat/latin9/
<
==============================================================================
6. Vimspell bugs  {{{2                                         *vimspell-bugs*

    I'm not able to reproduce most of those bug with last version of vim and
    vimspell. If someone has more information about them, I will welcome them.

    - BUG digraphs are not passed to aspell/ispell. Since iskeyword is set
      globally, they are not underlined in red... 
    - When not all errors are highlighted, SpellCheck() display an erroneous 
      error count.
    - problem when opening file with a swap files. Messages are not visible.
      to be reproduced...
    - ispell doesn't seem to work with word containing iso-8559 encoded
      characters in TeX files...
    - BUG reported by Fabio Stumbo <f.stumbo@unife.it>:
      Textual navigation in Plugin submenus doesn't work when pressing <F4>
      with the following .vimrc settings: >
 	   source $VIMRUNTIME/menu.vim
 	   set wildmenu
 	   set cpo-=<
 	   set wcm=<C-Z>
 	   map <F4> :emenu <C-Z>
<   - BUG reported by Rajarshi Guha. Vim 6.1 hang up with vimspell 1.46 when
      editing HTML (with EasyHtml 0.5.1 script) and sometimes when editing
      latex *with aspell*. Vim need kill -9. -- I'm not able to reproduced it
      with vimspell 1.65 (see also Fabio Stumbo bug below)...
    - BUG with aspell (aspell-0.33.7.1-7mdk) on HTML files where aspell seems
      to loop infinitely (Fabio Stumbo <f.stumbo@unife.it>). 
    - autocommands are not called in insert mode : this is apparently a
      feature of VIM. There will perhaps be a hook which will allow this in
      vim 6.2, but Bram seems quite reluctant in implementing it (because
      autocomands are dangerous and difficult to test thoroughly).

==============================================================================
7. Vimspell .TODO list  {{{2                                    *vimspell-todo*

    - Take into account the FileType event to setup buffer correctly.
    - Add a way to customize the spell checking (so as to not use the syntax
      highlighting as a way to determine was as to be checked). For example,
      with the current syntax files, html and xml are badly checked.
    - Change vimspell so that it works with csh (use of backslash_quote
      variable) ?
    - Add a command to spellcheck a visually selected region
    - Add options to prevent some words to be checked (like TODO). If not,
      their highlighting is overwritten by spellcheck's one (depends of TODO
      highlighting definition... To be investigated).
    - selection of syntax group for which spelling is done (for example, only
      string and comments are of interest in a C source code..) - Partly done.
    - ...
    - reduce this TODO list (I didn't think it would have grown so quickly).

==============================================================================
=== END_DOC
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" v im:tw=78:ts=8:ft=help:norl:
" vim600: set foldmethod=marker  tabstop=8 shiftwidth=2 softtabstop=2 smartindent smarttab  :
"fileencoding=iso-8859-15 
