#colorscheme zenburn
#hi NonText guifg=#555555 gui=NONE term=NONE
#hi clear LineNr
#hi link LineNr NonText

colorscheme oceandeep

"set fillchars=vert:│,fold:-,diff:┄,stl:\ ,stlnc:\ 
"set listchars=eol:¶,tab:»\ ,trail:·,extends:⇒

set guicursor+=a:blinkon0
set guioptions=aigm
set mousemodel=popup
set scrolloff=6
set sidescrolloff=6
set visualbell
set errorbells
set cursorline

if has("gui_macvim")
    macmenu &File.Open\.\.\. key=<nop>
    "macmenu &File.Close key=<nop>
    nmap <D-0> <C-w>c
    nmap <D-1> <C-w>o
    nmap <D-2> <C-w><C-s>
    nmap <D-3> <C-w><C-v>
    nmap <D-o> <C-w><C-w>
    imap <D-0> <Esc><C-w>c
    imap <D-1> <Esc><C-w>o
    imap <D-2> <Esc><C-w><C-s>
    imap <D-3> <Esc><C-w><C-v>
    imap <D-o> <Esc><C-w><C-w>
    imap <D-k> <Esc>:BD<CR>
endif

" now load specifics to this machine
source ~/.gvimlocal
