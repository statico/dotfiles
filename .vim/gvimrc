" Pro tip for MacVim on retina displays: Get the sexy font rendering used by Termial.app/iTerm:
" defaults write org.vim.MacVim AppleFontSmoothing -int 0

" Gutter color default is annoying. Make it blend in.
hi SignColumn guibg=NONE

hi Search term=NONE ctermfg=0 ctermbg=14 guifg=#ffffff guibg=#444444

" Command-Shift-F is fullscreen.
nmap <D-F> :set fullscreen!<CR>

set columns=80 lines=50
set errorbells
set fuoptions=maxhorz,maxvert
set guicursor+=a:blinkon0
set guifont=Essential\ PragmataPro:h15
set guioptions=aigm
set mousemodel=popup
set nocursorline
set nonumber
set scrolloff=6
set sidescrolloff=6
set transparency=0
set visualbell

" Now load specifics to this machine.
if filereadable(expand("~/.gvimlocal"))
  source ~/.gvimlocal
endif
