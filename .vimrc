" vim:fdm=marker:ts=4:sw=4:et:
"        _                    
" __   _(_)_ __ ___  _ __ ___ 
" \ \ / / | '_ ` _ \| '__/ __|
"  \ V /| | | | | | | | | (__ 
"   \_/ |_|_| |_| |_|_|  \___|
"                             
" Ian's .vimrc file
"
" Section: Key mappings {{{1
"--------------------------------------------------------------------------
nmap \a :set formatoptions-=a<CR>:echo "autowrap disabled"<CR>
nmap \A :set formatoptions+=a<CR>:echo "autowrap enabled"<CR>
nmap \b :set nocin tw=80<CR>:set formatoptions+=a<CR>
vmap \B <Esc>:!tabtable<CR>
nmap \c :%!indent<CR>
nmap \d :%!perltidy<CR>
nmap \e :e#<CR>
nmap \E :w<CR>:e#<CR>
nmap \f :let perl_fold=1<CR>:e<CR>:set fdl=1<CR>
nmap \g mt\d'tzz
nmap \i i<CR><CR>> <Esc>kkgqapj)
nmap \I S<Esc>kgqapo<CR><Up>
nmap \k :execute "!perl -c %"<CR>
nmap \l :set number!<CR>:set number?<CR>
nmap \M :set noexpandtab tabstop=2 softtabstop=2 shiftwidth=2<CR>
nmap \m :set expandtab tabstop=2 shiftwidth=2<CR>
nmap \N :next!<CR>
nmap \n :next<CR>
nmap \o :set paste!<CR>:set paste?<CR>
nmap \P :prev!<CR>
nmap \p :prev<CR>
nmap \q :nohlsearch<CR>
nmap \r mt?^-- $<CR>O<Esc>'tVG{kdO<Esc>kgqap:nohlsearch<CR>O
nmap \s :vertical resize 30<CR>
nmap \t :set expandtab tabstop=2 shiftwidth=2<CR>
nmap \T :set expandtab tabstop=2 shiftwidth=2<CR>
nmap \u :set list!<CR>:set list?<CR>
nmap \w :set wrap!<CR>:set wrap?<CR>
nmap \x :w<CR>:%! xmllint --format - <CR>
nmap \z :w<CR>:!<Up><CR>

nmap \0 :buffers<CR>
nmap \1 :e #1<CR>
nmap \2 :e #2<CR>
nmap \3 :e #3<CR>
nmap \4 :e #4<CR>
nmap \5 :e #5<CR>
nmap \6 :e #6<CR>
nmap \7 :e #7<CR>
nmap \8 :e #8<CR>
nmap \9 :e #9<CR>

" lookup word with meta-D
nmap <Esc>d :!links -dump 'http://www.dict.org/bin/Dict?Form=Dict1&Strategy=*&Database=*&Query=<cword>' \| less<CR>

" run the buffer in perl (^P is normally just the up arrow in normal mode)
nmap <M-p>  :!clear; prove -v %<CR>
nmap <Esc>p :!clear; prove -v %<CR>

" save my poor shift key
"nmap ; :

" CTRL-z is too close, plus I use it very frequently
nmap - :w<CR><C-z>

nmap <C-p>  :call RunUsingCurrentFiletype()<CR>
function! RunUsingCurrentFiletype()
    execute 'write'
    execute '! clear; '.&filetype.' <% '
endfunction

" Section: Hacks {{{1
"--------------------------------------------------------------------------

" Make j & k linewise {{{2

" turn off linewise keys -- normally, the `j' and `k' keys move the cursor down
" one entire line. with line wrapping on, this can cause the cursor to actually
" skip a few lines on the screen because it's moving from line N to line N+1 in
" the file. I want this to act more visually -- I want `down' to mean the next
" line on the screen
map j gj
map k gk

" having Ex mode start or showing me the command history
" is a complete pain in the ass if i mistype
map Q <silent>
map q: <silent>
map K <silent>
"map q <silent>

" Keyboard mapping for cursor keys on broken terminals {{{2
" [works for XTerminals - 970818]
map  <ESC>[A <Up>
map  <ESC>[B <Down>
map  <ESC>[C <Right>
map  <ESC>[D <Left>
imap <ESC>[A <Up>
imap <ESC>[B <Down>
imap <ESC>[C <Right>
imap <ESC>[D <Left>

" Allow multiple undos when creating a large edited section. {{{2
" http://www.vim.org/tips/tip.php?tip_id=1054
"" function! EnterStuff()   
""     let theLine = getline(line("."))   
""     let pos = col("'^")   
""     execute "normal mqu\<C-r>`q"   
""     if(pos > strlen(theLine))   
""         startinsert!   
""     else   
""         if(pos > 1)   
""             normal l   
""         endif   
""         startinsert   
""     endif   
"" endfunction   
"" function! ChangeUndoMode(theNum)   
""     if(a:theNum == 1)   
""         inoremap <C-w> <C-w>^O^[   
""         inoremap <Enter> <ESC>:call EnterStuff()<Enter><Enter>   
""     elseif(a:theNum == 2)   
""         inoremap <C-w> <C-w>^O^[   
""         inoremap <BS> <BS>^O^[   
""         inoremap <DEL> <DEL>^O^[   
""         inoremap <Enter> <ESC>:call EnterStuff()<Enter><Enter>   
""     else   
""         iunmap <C-w>   
""         iunmap <BS>   
""         iunmap <DEL>   
""         iunmap <Enter>   
""     endif   
"" endfunction   
"" call ChangeUndoMode(1)   
"nmap \sun :call ChangeUndoMode(1)<Enter>   
"nmap \gun :call ChangeUndoMode(2)<Enter>   
"nmap \bun :call ChangeUndoMode(3)<Enter>   

" Section: Abbrevations {{{1
"--------------------------------------------------------------------------

" Vim command line: $c 
" URL: http://www.vim.org/tips/tip.php?tip_id=1055
cno $c e <C-\>eCurrentFileDir()<CR>
function! CurrentFileDir()
   return "e " . expand("%:p:h") . "/"
endfunction

" Perl: warnings, strict, use Test::More
iab uw;   use strict;<CR>use warnings;<CR>
iab ubp;  <C-o>:set paste<CR>#!/usr/bin/env perl<CR>use strict;<CR>use warnings;<CR><C-o>:set nopaste<CR>
iab utm;  use Test::More 'no_plan';<CR>use strict;<CR>use warnings;<CR>

" Section: Vim options {{{1
"--------------------------------------------------------------------------

set autoindent              " carry over indenting from previous line
set backspace=2             " allow backspace beyond insertion point
set cindent                 " automatic program indenting
set cinkeys-=0#             " comments don't fiddle with indenting
set commentstring=\ \ #%s   " when folds are created, add them to this
set copyindent              " make autoindent use the same chars as prev line
set directory-=.            " don't store temp files in cwd
set encoding=utf8           " UTF-8 by default
set expandtab               " no tabs
set foldmethod=marker       " use braces by default
set formatoptions=tcqn1     " t - autowrap normal text
                            " c - autowrap comments
                            " q - gq formats comments
                            " n - autowrap lists
                            " 1 - break _before_ single-letter words
                            " 2 - use indenting from 2nd line of para
set hlsearch                " hilight searching
set ignorecase              " case insensitive
set incsearch               " search as you type
set infercase               " completion recognizes capitalization
set linebreak               " break long lines by word, not char
set listchars+=tab:\|\      " make tabs show up in 'list' mode
set matchtime=2             " tenths of second to hilight matching paren
set modelines=5             " how many lines of head & tail to look for ml's
set nobackup                " no backups left after done editing
set novisualbell            " no flashing
set nowritebackup           " no backups made while editing
set printoptions=paper:letter " US paper
set ruler                   " show row/col and percentage
set scroll=4                " number of lines to scroll with ^U/^D
set scrolloff=15            " keep cursor away from this many chars top/bot
set shiftround              " shift to certain columns, not just n spaces
set shiftwidth=2            " number of spaces to shift for autoindent or >,<
set textwidth=80            " Wrap lines after 80 chars
set showmatch               " when a bracket is inserted, jump to it
set sidescrolloff=3         " keep cursor away from this many chars left/right
set tabstop=2               " the One True Tab
set wildmenu                " show possible completions on command line
set wildmode=list:longest,full " list all options and complete

" enable filetype plugins -- e.g., ftplugin/xml.vim
filetype plugin on
filetype indent on

" Section: Commands {{{1
"--------------------------------------------------------------------------

" i always, ALWAYS hit ":W" instead of ":w"
command! Q q
command! W w

" insert date
command! DS r!date

" Section: Plugin settings {{{1
"--------------------------------------------------------------------------

" for any plugins that use this, make their keymappings use comma
let mapleader = ","
let maplocalleader = ","

" ftplugin/mail.vim
let no_plugin_maps = 0

" perl.vim
let perl_include_pod = 1

" Explore.vim (comes with Vim 6)
let explVertical = 1
let explSplitRight = 1
let explWinSize = 30
let explHideFiles = '^\.,\.(class|swp)$,^CVS$'
let explDirsFirst = -1

" vtreeexplorer.vim
let treeExplVertical = 1
let treeExplWinSize = 30
let treeExplDirSort = -1
unlet! treeExplHidden

" vimspell.vim
let spell_auto_type = ""

" cvscommand.vim
let CVSCommandDiffOpt = "" " let .cvsrc handle this

" taglist.vim
let Tlist_Use_Right_Window = 1
let Tlist_WinWidth = 30

" Section: Color and syntax {{{1
"--------------------------------------------------------------------------

" make sure colored syntax mode is on
if has("terminfo")
  set t_Co=8
  set t_Sf=[3%p1%dm
  set t_Sb=[4%p1%dm
else
  set t_Co=8
  set t_Sf=[3%dm
  set t_Sb=[4%dm
endif
syntax on
colorscheme default

" folded text was too bright - change to black on cyan bg
highlight Folded ctermfg=black ctermbg=cyan cterm=none

" window splits & ruler were too bright - change to white on grey
" (shouldn't change GUI or non-color term appearance)
highlight StatusLine   cterm=inverse ctermbg=black ctermfg=blue
highlight StatusLineNC cterm=NONE ctermbg=black ctermfg=white
highlight VertSplit    cterm=NONE ctermbg=black ctermfg=white

" unfortunately, taglist.vim's filenames is linked to LineNr, which sucks
highlight def link MyTagListFileName Statement
highlight def link MyTagListTagName Question

" turn off coloring for CDATA
highlight def link xmlCdata NONE

" custom incorrect spelling colors
highlight SpellErrors guifg=lightred guibg=bg gui=underline cterm=underline term=underline

" ignore should be... ignored
highlight Ignore cterm=bold ctermfg=black
highlight clear FoldColumn
highlight def link FoldColumn Ignore

" nice-looking hilight if I remember to set my terminal colors
highlight clear Search
highlight Search term=NONE cterm=NONE ctermfg=white ctermbg=black

" colors for vtreeexplorer
highlight treeDir cterm=none ctermfg=blue
highlight treeLnk cterm=bold ctermfg=cyan
highlight def link treePrt Ignore

" Section: Load ~/.vimlocal {{{1
"--------------------------------------------------------------------------

" now load specifics to this machine
source ~/.vimlocal

