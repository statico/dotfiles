" Description: Replace one word with another, keeping case.
" Author: Michael Geddes <vimmer at frog.wheelycreek.net>
" Date: 13th Mar 2007.
" Version: 2.0
" Patched by Luc Hermitte, on 13th Mar 2007.

" Usage: Using KeepCase or KeepCaseSameLen defined here, do a substitution like this:
" %s/\u\<old_word\>/\=KeepCaseSameLen(submatch(0), 'new_word')/g
" 
" * KeepCase( original_word , new_word )  
"   returns the new word maintaining case 
"   simply uses heuristics to work out some different common situations
"     given   NewWord
"     Word   	--> Newword
"     WORD    --> NEWWORD
"     word    --> newword
"     WoRd    --> NewWord
"     woRd    --> newWord
" 
" * KeepCaseSameLen( original_word , new_word )    
" 	Returns the new word maintaining case
" 	  Keeps the case exactly the same letter-for-letter
" 	  It does work if the words aren't the same length, as it truncates or
" 	  just coppies the case of the word for the length of the original word.
"
" * :SubstituteCase#\ctoto\(Titi\)tata#\1Tutu#g
"     totoTitiTata -> titiTutu
"     TotoTitiTata -> TitiTutu
"     tototititata -> tititutu
"     tototitiTata -> titiTutu
"     TototitiTata -> TitiTutu


let s:keep_case_m1='^\u\l\+$'
let s:keep_case_r1='^\(.\)\(.\+\)$'
let s:keep_case_s1='\u\1\L\2'

let s:keep_case_m2='^\u\+$'
let s:keep_case_r2='^.\+$'
let s:keep_case_s2='\U&'

let s:keep_case_m3='^\l\+$'
let s:keep_case_r3='^.\+$'
let s:keep_case_s3='\L&'

let s:keep_case_m4='^\u.\+$'
let s:keep_case_r4='^.\+$'
let s:keep_case_s4='\u&'

let s:keep_case_m5='^\l.\+$'
let s:keep_case_r5='^.\+$'
let s:keep_case_s5='\l&'

fun! KeepCase( oldword, newword)
  let n=1
  while n <=5
	exe 'let mx = s:keep_case_m'.n
	if a:oldword =~ mx
	  exe 'let rx = s:keep_case_r'.n
	  exe 'let sx = s:keep_case_s'.n
	  return substitute(a:newword,rx,sx,'')
	endif
	let n += 1
  endwhile
  return a:newword
endfun
"echo KeepCase('test','FreDrick')
"echo KeepCase('Test','FrEdRick')
"echo KeepCase('teSt','FrEdrIck')
"echo KeepCase('TeSt','FrEdrIck')
"echo KeepCase('TEST','FredRick')

fun! KeepCaseSameLen( oldword, newword)
  let ret=''
  let i=0
  let len=strlen(a:oldword)
  while i<len
	if a:oldword[i] =~ '\u'
	  let ret .= substitute(a:newword[i],'.','\u&','')
	elseif a:oldword[i] =~ '\l'
	  let ret .= substitute(a:newword[i],'.','\l&','')
	else
	  let ret .= a:newword[i]
	endif
	let i += 1
  endwhile
  let ret .= strpart(a:newword,i,strlen(a:newword))
  return ret
endfun

" LH's additions (13th Mar 2007)
function! s:SubstituteCase(repl_arg) range
  let sep = a:repl_arg[0]
  let args = split(a:repl_arg, sep)
  " Main keepcase
  let args[1] = '\=KeepCase(submatch(0), ' . "'" . args[1] . "')"
  " Take care of back-references
  let args[1] = substitute(args[1], '\\\(\d\+\)', "'.submatch(\\1).'", 'g')

  let cmd = a:firstline . ',' . a:lastline . 's' . sep . join(args, sep)
  exe cmd
endfunction

:command! -nargs=1 -range SubstituteCase 
      \ <line1>,<line2>call s:SubstituteCase(<f-args>)
