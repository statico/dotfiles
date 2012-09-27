" Vim colour file
" Maintainer: Matthew Hawkins <matt@mh.dropbear.id.au>
" Last Change:  Mon, 22 Apr 2002 15:28:04 +1000
" URI: http://mh.dropbear.id.au/vim/navajo-night.png
"
" This colour scheme uses a "navajo-black" background
" I have added colours for the statusbar and for spell checking
" as taken from Cream (http://cream.sf.net/)


set background=dark
hi clear
if exists("syntax_on")
  syntax reset
endif

let g:colors_name = "navajo-night"

" This is the list of colour changes from Navajo that
" weren't a simple mathematical subtraction from 0xffffff
" DarkBlue -> #ffff74
" DarkRed -> #74ffff
" DarkGreen -> #ff9bff
" DarkCyan -> #ff7474
" DarkMagenta -> #74ff74
" DarkYellow -> #7474ff
" DarkGray -> #565656
" Blue -> Yellow
" Red -> Cyan
" Yellow -> Blue
" Gray -> #414141
" Brown -> #5ad5d5
" #ff8060 -> #007f9f
" #f6e8d0 -> #09172f
" #edb5cd -> #124a32
" #c0c0c0 -> #3f3f3f
" #907050 -> #6f8faf
" #808080 -> #7f7f7f
" #707070 -> #8f8f8f
" SeaGreen -> #d174a8
" LightRed (assuming #ee9090) -> #116f6f
" LightBlue -> #522719

hi Normal guifg=White guibg=#35536f

hi SpecialKey guifg=Yellow
hi NonText gui=bold guifg=#7f7f7f
hi Directory guifg=Yellow
hi ErrorMsg gui=bold guifg=Black guibg=Cyan
hi IncSearch gui=reverse
hi Search guibg=Black guifg=Yellow
hi MoreMsg gui=bold guifg=#d174a8
hi ModeMsg gui=bold
hi LineNr guibg=#7f7f7f gui=bold guifg=White
hi Question gui=bold guifg=#d174a8
hi StatusLine gui=bold guifg=Black guibg=White
hi StatusLineNC gui=bold guifg=#116f6f guibg=#8f8f8f
hi VertSplit gui=bold guifg=Black guibg=#8f8f8f
hi Title gui=bold guifg=#74ff74
"+++ Cream:
"hi Visual gui=reverse guifg=#3f3f3f guibg=White
"+++
hi VisualNOS gui=reverse guifg=#414141 guibg=Black
hi WarningMsg gui=bold guifg=Cyan
hi WildMenu guifg=White guibg=Blue
hi Folded guifg=White guibg=NONE guifg=#afcfef
hi FoldColumn guifg=#ffff74 guibg=#3f3f3f
hi DiffAdd guibg=Black
hi DiffChange guibg=#124a32
hi DiffDelete gui=bold guifg=#522719 guibg=#09172f
hi DiffText gui=bold guibg=#007f9f
hi Cursor gui=reverse guifg=#bfbfef guibg=Black
hi lCursor guifg=fg guibg=bg
hi Match gui=bold,reverse guifg=Blue guibg=Yellow


" Colours for syntax highlighting
hi Comment guifg=#e7e77f
hi Constant guifg=#3fffa7
hi Special guifg=#bfbfef
hi Identifier guifg=#ef9f9f
hi Statement gui=bold guifg=#5ad5d5
hi PreProc guifg=#74ff74
hi Type gui=bold guifg=#d174a8
hi Ignore guifg=bg

hi Error gui=bold guifg=Black guibg=Cyan
hi Todo guifg=Yellow guibg=Blue

"+++ Cream: statusbar
" Colours for statusbar
"hi User1        gui=bold guifg=#565656  guibg=#0c0c0c
"hi User2        gui=bold guifg=White     guibg=#0c0c0c
"hi User3        gui=bold guifg=Yellow      guibg=#0c0c0c
"hi User4        gui=bold guifg=Cyan       guibg=#0c0c0c
highlight User1        gui=bold guifg=#999933  guibg=#45637f
highlight User2        gui=bold guifg=#e7e77f     guibg=#45637f
highlight User3        gui=bold guifg=Black      guibg=#45637f
highlight User4        gui=bold guifg=#33cc99       guibg=#45637f
"+++

"+++ Cream: selection
highlight Visual    gui=bold    guifg=Black guibg=#aacc77
"+++

"+++ Cream: bookmarks
highlight Cream_ShowMarksHL guifg=Black guibg=#aacc77 gui=bold
"+++

"+++ Cream: spell check
" Colour misspelt words
"hi BadWord guifg=Yellow guibg=#522719 gui=bold
" mathematically correct:
"highlight BadWord gui=NONE guifg=White guibg=#003333
" adjusted:
highlight BadWord gui=NONE guifg=#ff9999 guibg=#003333
"+++


