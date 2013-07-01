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

" useful macros I use the most
nmap \a :set formatoptions-=a<CR>:echo "autowrap disabled"<CR>
nmap \A :set formatoptions+=a<CR>:echo "autowrap enabled"<CR>
nmap \b :set nocin tw=80<CR>:set formatoptions+=a<CR>
nmap \c :CoffeeCompile watch<CR>
nmap \d :%!perltidy<CR>
nmap \e :NERDTreeToggle<CR>
nmap \g :Gstatus<CR>
nmap \l :setlocal number!<CR>:setlocal number?<CR>
nmap \M :set noexpandtab tabstop=8 softtabstop=4 shiftwidth=4<CR>
nmap \m :set expandtab tabstop=2 shiftwidth=2 softtabstop=2<CR>
nmap \o :set paste!<CR>:set paste?<CR>
nmap \q :nohlsearch<CR>
nmap \s :setlocal invspell<CR>
nmap \t :set expandtab tabstop=4 shiftwidth=4 softtabstop=4<CR>
nmap \T :set expandtab tabstop=8 shiftwidth=8 softtabstop=4<CR>
nmap \u :setlocal list!<CR>:setlocal list?<CR>
nmap \w :setlocal wrap!<CR>:setlocal wrap?<CR>
nmap \x :w<CR>:%! xmllint --format - <CR>
nmap \Y :vertical resize 40<CR>:wincmd l<CR>
nmap \y :exec "vertical resize " . (80 + (&number * &numberwidth))<CR>:wincmd l<CR>
nmap \z :w<CR>:!<Up><CR>

" You don't know what you're missing if you don't use this.
nmap <C-e> :e#<CR>

" Move between open buffers.
map <C-n> :bnext<CR>
map <C-p> :bprev<CR>

" Let's try buffkill-vim using my favorite Emacs binding...
nmap <Esc>k :BD<CR>
nmap <M-k> :BD<CR>
nmap <D-k> :BD<CR>

" Emacs-like bindings in normal mode
nmap <C-x>0 <C-w>c
nmap <C-x>1 <C-w>o
nmap <C-x>1 <C-w>s
nmap <C-x>1 <C-w>v
nmap <C-x>o <C-w><C-w>
nmap <M-o> <C-w><C-w>

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

map <C-j> <C-W>j
map <C-k> <C-W>k
map <C-h> <C-W>h
map <C-l> <C-W>l

" Search for the word under the cursor in the current directory
nmap <C-k> :Ag "\b<cword>\b" <CR>

" Alt-p pipes the current buffer to the current filetype as a command
" (good for perl, python, ruby, shell, gnuplot...)
nmap <M-p>  :call RunUsingCurrentFiletype()<CR>
nmap <Esc>p :call RunUsingCurrentFiletype()<CR>
function! RunUsingCurrentFiletype()
    execute 'write'
    execute '! clear; '.&filetype.' <% '
endfunction

" Hex mode from http://vim.wikia.com/wiki/Improved_hex_editing
" ex command for toggling hex mode - define mapping if desired
command -bar Hexmode call ToggleHex()

" helper function to toggle hex mode
function ToggleHex()
  " hex mode should be considered a read-only operation
  " save values for modified and read-only for restoration later,
  " and clear the read-only flag for now
  let l:modified=&mod
  let l:oldreadonly=&readonly
  let &readonly=0
  let l:oldmodifiable=&modifiable
  let &modifiable=1
  if !exists("b:editHex") || !b:editHex
    " save old options
    let b:oldft=&ft
    let b:oldbin=&bin
    " set new options
    setlocal binary " make sure it overrides any textwidth, etc.
    let &ft="xxd"
    " set status
    let b:editHex=1
    " switch to hex editor
    %!xxd
  else
    " restore old options
    let &ft=b:oldft
    if !b:oldbin
      setlocal nobinary
    endif
    " set status
    let b:editHex=0
    " return to normal editing
    %!xxd -r
  endif
  " restore values for modified and read only state
  let &mod=l:modified
  let &readonly=l:oldreadonly
  let &modifiable=l:oldmodifiable
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

" Section: Abbrevations {{{1
"--------------------------------------------------------------------------

" Vim command line: $c
" URL: http://www.vim.org/tips/tip.php?tip_id=1055
cno $c e <C-\>eCurrentFileDir()<CR>
function! CurrentFileDir()
   return "e " . expand("%:p:h") . "/"
endfunction

" Section: Vim options {{{1
"--------------------------------------------------------------------------

set autoindent              " Carry over indenting from previous line
set autoread                " Don't bother me hen a file changes
set autowrite               " Write on :next/:prev/^Z
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
set laststatus=2            " Always show the status bar
set linebreak               " Break long lines by word, not char
set listchars=tab:▶\ ,trail:◀,extends:»,precedes:«
                            " Unicode characters for various things
set matchtime=2             " Tenths of second to hilight matching paren
set modelines=5             " How many lines of head & tail to look for ml's
silent! set mouse=nvc       " Use the mouse, but not in insert mode
set nobackup                " No backups left after done editing
set nonumber                " No line numbers to start
set visualbell t_vb=        " No flashing or beeping at all
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
set wildignore=*.class,*.o,*~,*.pyc,.git,node_modules  " Ignore certain files in tab-completion

" Section: Commands & Functions {{{1
"--------------------------------------------------------------------------

" i always, ALWAYS hit ":W" instead of ":w"
command! Q q
command! W w

" http://stackoverflow.com/questions/1005/getting-root-permissions-on-a-file-inside-of-vi
cmap w!! w !sudo tee >/dev/null %

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

" taglist.vim
let Tlist_Use_Right_Window = 1
let Tlist_WinWidth = 30

" NERD_tree.vim
let NERDTreeIgnore = ['\~$', '\.pyc$']

" ctrlp.vim (replaces FuzzyFinder and Command-T)
let g:ctrlp_map = '<Leader>t'
let g:ctrlp_match_window_bottom = 0
let g:ctrlp_match_window_reversed = 0
let g:ctrlp_custom_ignore = '\v\~$|\.(o|swp|pyc|wav|mp3|ogg|blend)$|(^|[/\\])\.(hg|git|bzr)($|[/\\])|__init__\.py'
let g:ctrlp_working_path_mode = 0
let g:ctrlp_dotfiles = 0
let g:ctrlp_switch_buffer = 0
nmap ; :CtrlPBuffer<CR>

" Powerline
"let g:Powerline_symbols = "unicode"

" Syntastic
let g:syntastic_enable_signs=1
let g:syntastic_auto_jump=0
let g:syntastic_stl_format = '[%E{Err: %fe #%e}%B{, }%W{Warn: %fw #%w}]'

" GitGutter
let g:gitgutter_sign_added = '∙'
let g:gitgutter_sign_modified = '∙'
let g:gitgutter_sign_removed = '∙'
let g:gitgutter_sign_modified_removed = '∙'

" enable filetype plugins -- e.g., ftplugin/xml.vim
filetype plugin indent on

" Section: Color and syntax {{{1
"--------------------------------------------------------------------------

" Helper to initialize Zenburn colors in 256-color mode.
function! ColorTermZenburn()
  colorscheme zenburn
  highlight Normal ctermbg=234
  let g:zenburn_high_Contrast = 1
endfunction

" Make sure colored syntax mode is on, and make it Just Work with newer 256
" color terminals like iTerm2.
if !has('gui_running')
  if $TERM == "xterm-256color" || $TERM == "screen-256color" || $COLORTERM == "gnome-terminal"
    set t_Co=256
    call ColorTermZenburn()
  elseif has("terminfo")
    colorscheme default
    set t_Co=8
    set t_Sf=[3%p1%dm
    set t_Sb=[4%p1%dm
  else
    colorscheme default
    set t_Co=8
    set t_Sf=[3%dm
    set t_Sb=[4%dm
  endif
endif
syntax on

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
highlight clear MatchParen
highlight link MatchParen Search

" colors for NERD_tree
highlight def link NERDTreeRO NERDTreeFile

" make trailing spaces visible
highlight SpecialKey ctermbg=Yellow guibg=Yellow

" make menu selections visible
highlight PmenuSel ctermfg=black ctermbg=magenta

" the sign column slows down remote terminals
highlight clear SignColumn
highlight link SignColumn Ignore

" Markdown could be more fruit salady.
highlight link markdownH1 PreProc
highlight link markdownH2 PreProc
highlight link markdownLink Character
highlight link markdownBold String
highlight link markdownItalic Statement
highlight link markdownCode Delimiter
highlight link markdownCodeBlock Delimiter
highlight link markdownListMarker Todo

" Section: Load ~/.vimlocal {{{1
"--------------------------------------------------------------------------

" now load specifics to this machine
source ~/.vimlocal
