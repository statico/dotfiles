" Vim syntax file
" Language:     RoboTalk
" Maintainer:   Ian Langworth <ian@langworth.com>
" Last Change:  2010-06-21

if version < 600
  syntax clear
elseif exists("b:current_syntax")
  finish
endif

syn case ignore

syn keyword rtVar AIM LOOK RADAR RANGE SCAN
syn keyword rtVar BULLET FIRE HELLBORE MINE MISSILE NUKE STUNNER
syn keyword rtVar BOTTOM BOT LEFT RIGHT TOP COLLISION WALL
syn keyword rtVar CHANNEL FRIEND SIGNAL TEAMMATES
syn keyword rtVar CHRONON DAMAGE DOPPLER ENERGY HISTORY ID KILLS PROBE ROBOTS SHIELD
syn keyword rtVar MOVEX MOVEY SPEEDX SPEEDY
syn keyword rtVar RANDOM
syn keyword rtVar A B C D E F G H I J K L M N O P Q R S T U V W X Y Z

syn keyword rtOp + - * /
syn keyword rtOp ABS CHS ARCCOS ARCSIN ARCTAN DIST COS COSINE MAX MIN
syn keyword rtOp MOD SIN SINE SQRT TAN TANGENT
syn keyword rtOp = !  > <
syn keyword rtOp XOR EOR NOT OR AND
syn keyword rtOp BEEP DEBUG DEBUGGER PRINT
syn keyword rtOp NOP CALL IF IFE IFG IFEG JUMP RETURN SYNC
syn keyword rtOp DROP DROPALL DUPLICATE DUP ROLL SWAP
syn keyword rtOp RECALL STO STORE VSTORE VRECALL
syn keyword rtOp FLUSHINT INTOFF INTON SETINT SETPARAM RTI
syn keyword rtOp ICON0 ICON1 ICON2 ICON3 ICON4 ICON5 ICON6 ICON7 ICON8 ICON9
syn keyword rtOp SND0 SND1 SND2 SND3 SND4 SND5 SND6 SND7 SND8 SND9

syn match rtNumber /[+-]\?[0-9]\+/
syn match rtStoredValue /\w\+'/
syn match rtLineComment  /#.*/
syn region rtLongComment  start="{" end="}"
syn match rtLabel /\w\+:/

"------------------------------------------------------------------------------

hi def link rtVar Type
hi def link rtOp Keyword

hi def link rtNumber Number
hi def link rtStoredValue Identifier
hi def link rtLabel Function

hi def link rtLineComment Comment
hi def link rtLongComment Comment

