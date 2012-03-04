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

" weird stuff that might confuse other Vim regulars
nnoremap ' `
nnoremap ` '

" useful macros I use the most
nmap \a :set formatoptions-=a<CR>:echo "autowrap disabled"<CR>
nmap \A :set formatoptions+=a<CR>:echo "autowrap enabled"<CR>
nmap \b :set nocin tw=80<CR>:set formatoptions+=a<CR>
vmap \B <Esc>:!tabtable<CR>
nmap \c mt:%!indent -kr --no-tabs<CR>'t
nmap \d :%!perltidy<CR>
nmap \e :TMiniBufExplorer<CR>
nmap \f :source ~/.vim/plugin/jpythonfold.vim<CR>
nmap \g mt\d't
nmap \h mt\H't
nmap \H :%!tidy -icq -asxhtml -w 80 -utf8<CR>
nmap \i i<CR><CR>> <Esc><Up><Up>gqapo<CR><Up>
nmap \I S<Esc><Up>gqapo<CR><Up>
nmap \j :CMiniBufExplorer<CR>:NERDTreeToggle<CR>
nmap \k :TlistToggle<CR>
nmap \l :setlocal number!<CR>:setlocal number?<CR>
nmap \M :set noexpandtab tabstop=8 softtabstop=4 shiftwidth=4<CR>
nmap \m :set expandtab tabstop=2 shiftwidth=2 softtabstop=2<CR>
nmap \o :set paste!<CR>:set paste?<CR>
nmap \q :nohlsearch<CR>
"nmap \r mt?^-- $<CR>O<Esc>'tVG{kdO<Esc>kgqap:nohlsearch<CR>O
"nmap \r mto<Esc>'tVG{kdO<Esc>kgqap:nohlsearch<CR>O
nmap \r ggj<C-v>}kecsquash<Esc>k0
nmap \s :setlocal invspell<CR>
nmap \t :set expandtab tabstop=4 shiftwidth=4 softtabstop=4<CR>
nmap \T :set expandtab tabstop=8 shiftwidth=8 softtabstop=4<CR>
nmap \u :setlocal list!<CR>:setlocal list?<CR>
nmap \v :call ToggleMiniBufVerticalness()<CR>
nmap \w :setlocal wrap!<CR>:setlocal wrap?<CR>
nmap \x :w<CR>:%! xmllint --format - <CR>
nmap \Y :vertical resize 40<CR>:wincmd l<CR>
nmap \y :exec "vertical resize " . (80 + (&number * &numberwidth))<CR>:wincmd l<CR>
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

" You don't know what you're missing if you don't use this.
nmap <C-e> :e#<CR>

" Move between open buffers.
map <C-n> :bnext<CR>
map <C-p> :bprev<CR>

" Let's try buffkill-vim using my favorite Emacs binding...
nmap <Esc>k :BD<CR>

" Let's try this new FuzzyFinder plugin...
nmap ; :FufBuffer<CR>
nmap <Leader>r :FufTag<CR>
nmap <D-e> :FufFile<CR>
nmap <M-e> :FufFile<CR>

" Let's try replacing FuzzyFinder with Command-T...
"nmap ; :CommandT<CR>

" Emacs-like bindings in normal mode
nmap <C-x>o <C-w><C-w>
nmap <C-x>0 <C-w>c
nmap <C-x>1 <C-w>o
nmap <C-x>1 <C-w>s
nmap <C-x>1 <C-w>v

" Emacs-like bindings in insert mode
imap <C-e> <C-o>$

" Emacs-like bindings in command line
cnoremap <C-a>  <Home>
cnoremap <C-b>  <Left>
cnoremap <C-f>  <Right>
cnoremap <C-d>  <Delete>
cnoremap <M-b>  <S-Left>
cnoremap <M-f>  <S-Right>
cnoremap <M-d>  <S-right><Delete>
cnoremap <Esc>b <S-Left>
cnoremap <Esc>f <S-Right>
cnoremap <Esc>d <S-right><Delete>
cnoremap <C-g>  <C-c>

" Why not use the space or return keys to toggle folds?
nnoremap <space> za
nnoremap <CR> za
vnoremap <space> zf

" Search for the word under the cursor in the current directory
nmap <C-k> :!clear; ack -C "\b<cword>\b" \| less -FRX <CR>

" CTRL-z is too close, plus I use it very frequently, and i want to write
" first
if !has('gui_running')
    nmap - :call WriteAndSuspend()<CR>
endif
function! WriteIfPossible()
    if &buftype != 'hidden' && !&readonly
        if bufname('%') != ""
            execute 'write'
        endif
    endif
endfunction
function! WriteAndSuspend()
    call WriteIfPossible()
    if $UID != 0
        " No idea why loading this vimrc as root causes this function to
        " automatically suspend at startup on some Vims. NO IDEA.
        suspend
    endif
endfunction

" Alt-p pipes the current buffer to the current filetype as a command
" (good for perl, python, ruby, shell, gnuplot...)
nmap <M-p>  :call RunUsingCurrentFiletype()<CR>
nmap <Esc>p :call RunUsingCurrentFiletype()<CR>
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

" Make the cursor stay on the same line when window switching {{{2

function! KeepCurrentLine(motion)
    let theLine = line('.')
    let theCol = col('.')
    exec 'wincmd ' . a:motion
    if &diff
        call cursor(theLine, theCol)
    endif
endfunction

nnoremap <C-w>h :call KeepCurrentLine('h')<CR>
nnoremap <C-w>l :call KeepCurrentLine('l')<CR>


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
iab ubp;  <C-o>:set paste<CR>#!/usr/bin/perl<CR>use strict;<CR>use warnings;<CR><C-o>:set nopaste<CR>
iab utm;  use Test::More 'no_plan';<CR>use strict;<CR>use warnings;<CR>

" Section: Vim options {{{1
"--------------------------------------------------------------------------

set autoindent              " Carry over indenting from previous line
set autoread                " Don't bother me hen a file changes
set backspace=indent,eol,start
                            " Allow backspace beyond insertion point
set cindent                 " Automatic program indenting
set cinkeys-=0#             " Comments don't fiddle with indenting
set cino=(0                 " Indent newlines after opening parenthesis
set commentstring=\ \ #%s   " When folds are created, add them to this
set copyindent              " Make autoindent use the same chars as prev line
set directory-=.            " Don't store temp files in cwd
set encoding=utf8           " UTF-8 by default
set expandtab               " No tabs
set fileformats=unix,dos,mac  " Prefer Unix
set fillchars=vert:\ ,stl:\ ,stlnc:\ ,fold:-,diff:┄
                            " Unicode chars for diffs/folds, and rely on
                            " Colors for window borders
silent! set foldmethod=marker " Use braces by default
set formatoptions=tcqn1     " t - autowrap normal text
                            " c - autowrap comments
                            " q - gq formats comments
                            " n - autowrap lists
                            " 1 - break _before_ single-letter words
                            " 2 - use indenting from 2nd line of para
set hidden                  " Don't prompt to save hidden windows until exit
set history=200             " How many lines of history to save
set hlsearch                " Hilight searching
set ignorecase              " Case insensitive
set incsearch               " Search as you type
set infercase               " Completion recognizes capitalization
set linebreak               " Break long lines by word, not char
set list                    " Show invisble characters in listchars
set listchars=tab:▶\ ,trail:◀,extends:»,precedes:«
                            " Unicode characters for various things
set matchtime=2             " Tenths of second to hilight matching paren
set modelines=5             " How many lines of head & tail to look for ml's
silent! set mouse=nvc       " Use the mouse, but not in insert mode
set nobackup                " No backups left after done editing
set novisualbell            " No flashing
set nowritebackup           " No backups made while editing
set printoptions=paper:letter " US paper
set ruler                   " Show row/col and percentage
set scroll=4                " Number of lines to scroll with ^U/^D
set scrolloff=15            " Keep cursor away from this many chars top/bot
set shiftround              " Shift to certain columns, not just n spaces
set shiftwidth=4            " Number of spaces to shift for autoindent or >,<
set shortmess+=A            " Don't bother me when a swapfile exists
set showbreak=              " Show for lines that have been wrapped, like Emacs
set showmatch               " Hilight matching braces/parens/etc.
set sidescrolloff=3         " Keep cursor away from this many chars left/right
set smartcase               " Lets you search for ALL CAPS
set softtabstop=4           " Spaces 'feel' like tabs
set suffixes+=.pyc          " Ignore these files when tab-completing
set tabstop=4               " The One True Tab
set notitle                 " Don't set the title of the Vim window
set wildmenu                " Show possible completions on command line
set wildmode=list:longest,full " List all options and complete
set wildignore=*.class,*.o,*~,*.pyc,.git,third_party,node_modules  " Ignore certain files in tab-completion

" Section: Commands & Functions {{{1
"--------------------------------------------------------------------------

" i always, ALWAYS hit ":W" instead of ":w"
command! Q q
command! W w

" insert date
command! DS r!date

" trim spaces at EOL
command! TEOL %s/ \+$//
command! CLEAN retab | TEOL

" hightlight more than 80 characters
function! HighlightTooLongLines()
  highlight def link RightMargin Error
  if &textwidth != 0
    exec 'match RightMargin /\%<' . (&textwidth + 4) . 'v.\%>' . (&textwidth + 2) . 'v/'
  endif
endfunction

" Rename.vim  -  Rename a buffer within Vim and on the disk
" Copyright June 2007 by Christian J. Robinson <infynity@onewest.net>
" Distributed under the terms of the Vim license.  See ":help license".
" http://www.infynity.spodzone.com/vim/Rename.vim
" Usage: :Rename[!] {newname}
command! -nargs=* -complete=file -bang Rename :call Rename("<args>", "<bang>")
function! Rename(name, bang)
    let l:curfile = expand("%:p")
    let v:errmsg = ""
    silent! exe "saveas" . a:bang . " " . a:name
    if v:errmsg =~# '^$\|^E329'
        if expand("%:p") !=# l:curfile && filewritable(expand("%:p"))
            silent exe "bwipe! " . l:curfile
            if delete(l:curfile)
                echoerr "Could not delete " . l:curfile
            endif
        endif
    else
        echoerr v:errmsg
    endif
endfunction


" Display output of shell commands in new window
" http://vim.wikia.com/wiki/Display_output_of_shell_commands_in_new_window
" Modified by ian to reuse the same window
command! -complete=shellcmd -nargs=+ Shell call s:RunShellCommand(<q-args>)
function! s:RunShellCommand(cmdline)

  if exists('s:ShellCommandBuffer')
    if bufloaded(s:ShellCommandBuffer)
      exec 'bwipeout '.s:ShellCommandBuffer
    endif
  endif

  echo a:cmdline
  let expanded_cmdline = a:cmdline
  for part in split(a:cmdline, ' ')
     if part[0] =~ '\v[%#<]'
        let expanded_part = fnameescape(expand(part))
        let expanded_cmdline = substitute(expanded_cmdline, part, expanded_part, '')
     endif
  endfor

  vertical botright new
  let s:ShellCommandBuffer = bufnr('%')
  setlocal buftype=nofile bufhidden=wipe nobuflisted noswapfile nowrap

  call setline(1, 'You entered:    ' . a:cmdline)
  call setline(2, 'Expanded Form:  ' .expanded_cmdline)
  call setline(3,substitute(getline(2),'.','=','g'))
  execute '$read !'. expanded_cmdline

  "setlocal nomodifiable
  1
endfunction


" Section: Python specifics {{{1
"--------------------------------------------------------------------------

if has('python')
python << EOF
import os
import sys
sys.path.append(os.path.join(os.getenv('HOME'), '.vim', 'python'))
EOF
endif

" Section: Plugin settings {{{1
"--------------------------------------------------------------------------

" A new Vim package system
runtime bundle/vim-pathogen/autoload/pathogen.vim
call pathogen#infect()
call pathogen#helptags()

" for any plugins that use this, make their keymappings use comma
let mapleader = ","
let maplocalleader = ","

" ftplugin/mail.vim
let no_plugin_maps = 0

" perl.vim
let perl_include_pod = 1

" perldoc
let g:perldoc_program='perldoc'

" Explore.vim (comes with Vim 6)
let explVertical = 1
let explSplitRight = 1
let explWinSize = 30
let explHideFiles = '^\.,\.(class|swp|pyc|pyo)$,^CVS$'
let explDirsFirst = -1

" vimspell.vim
let spell_auto_type = ""

" cvscommand.vim
let CVSCommandDiffOpt = "" " let .cvsrc handle this

" taglist.vim
let Tlist_Use_Right_Window = 1
let Tlist_WinWidth = 30

" guifont++.vim
let guifontpp_smaller_font_map="<M-->"
let guifontpp_larger_font_map="<M-+>"
let guifontpp_original_font_map="<M-=>"

" minibufexpl.vim
function! ToggleMiniBufVerticalness()
    let g:miniBufExplVSplit = g:miniBufExplVSplit ? 0 : 30
    silent! TMiniBufExplorer
    silent! TMiniBufExplorer
endfunction
"let g:miniBufExplorerMoreThanOne=999 " Don't open MBE by default
"let g:miniBufExplorerMoreThanOne=3 " Show when 3+ buffers are open.

" NERD_tree.vim
let NERDTreeIgnore = ['\~$', '\.pyc$']

" fuf.vim
let g:fuf_infoFile = ''
let g:fuf_file_exclude = '\v\~$|\.(o|swp|pyc)$|(^|[/\\])\.(hg|git|bzr)($|[/\\])|__init__\.py'
let g:fuf_keyNextMode = '<C-y>'
let g:fuf_keyPrevMode = '<C-t>'

" Command-T
let g:CommandTMatchWindowAtTop = 1
let g:CommandTCancelMap = ['<C-g>', '<C-c>', '<Esc>']

" Syntastic
let g:syntastic_enable_signs=1
let g:syntastic_auto_jump=0
let g:syntastic_stl_format = '[%E{Err: %fe #%e}%B{, }%W{Warn: %fw #%w}]'
"set statusline+=%#warningmsg#
"set statusline+=%{SyntasticStatuslineFlag()}
"set statusline+=%*

" enable filetype plugins -- e.g., ftplugin/xml.vim
filetype plugin indent on


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

" window splits & ruler were too bright - change to white on grey
" (shouldn't change GUI or non-color term appearance)
highlight StatusLine   cterm=NONE ctermbg=blue ctermfg=white
highlight StatusLineNC cterm=NONE ctermbg=black ctermfg=white
highlight VertSplit    cterm=NONE ctermbg=black ctermfg=white

" unfortunately, taglist.vim's filenames is linked to LineNr, which sucks
highlight def link MyTagListFileName Statement
highlight def link MyTagListTagName Question

" turn off coloring for CDATA
highlight def link xmlCdata NONE

" custom incorrect spelling colors
highlight SpellBad     term=underline cterm=underline ctermbg=NONE ctermfg=red
highlight SpellCap     term=underline cterm=underline ctermbg=NONE ctermfg=blue
highlight SpellRare    term=underline cterm=underline ctermbg=NONE ctermfg=magenta
highlight SpellLocal   term=underline cterm=underline ctermbg=NONE ctermfg=cyan

" ignore should be... ignored
highlight Ignore cterm=bold ctermfg=black
highlight clear FoldColumn
highlight def link FoldColumn Ignore
highlight clear Folded
highlight link Folded Ignore
highlight clear LineNr
highlight! def link LineNr Ignore

" nice-looking hilight if I remember to set my terminal colors
highlight clear Search
highlight Search term=NONE cterm=NONE ctermfg=white ctermbg=black

" make hilighted matching parents less offensive
highlight MatchParen term=NONE cterm=NONE ctermfg=black ctermbg=green

" colors for NERD_tree
highlight treeDir cterm=none ctermfg=blue
highlight treeLink cterm=bold ctermfg=cyan
highlight treeExecFile cterm=bold ctermfg=green
highlight def link treePart Ignore
highlight def link treePartFile Ignore
highlight def link treeClosable Ignore
highlight def link treeOpenable Ignore
highlight def link treeFlag Ignore
highlight def link treeDirSlash treeDir
highlight def link treeRO treeFile

" make trailing spaces visible
highlight SpecialKey ctermbg=Yellow guibg=Yellow

" make menu selections visible
highlight PmenuSel ctermfg=black ctermbg=magenta

" the sign column slows down remote terminals
highlight clear SignColumn
highlight link SignColumn Ignore

" Section: Load ~/.vimlocal {{{1
"--------------------------------------------------------------------------

" now load specifics to this machine
source ~/.vimlocal
