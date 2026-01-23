" Neovim Configuration
" Base setup - build from here

" ----------------------------------------------------------------------------
" PLUGIN MANAGER: vim-plug
" ----------------------------------------------------------------------------

" Install vim-plug if not present
if empty(glob('~/.local/share/nvim/site/autoload/plug.vim'))
  silent !curl -fLo ~/.local/share/nvim/site/autoload/plug.vim --create-dirs
    \ https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim
  autocmd VimEnter * PlugInstall --sync | source $MYVIMRC
endif

call plug#begin('~/.local/share/nvim/plugged')

" Functionality
Plug 'airblade/vim-gitgutter'
Plug 'docunext/closetag.vim'
Plug 'ervandew/supertab'
Plug 'haya14busa/incsearch.vim'
Plug 'junegunn/fzf', { 'dir': '~/.fzf', 'do': './install --all' }
Plug 'junegunn/fzf.vim'
Plug 'junegunn/goyo.vim'
Plug 'junegunn/limelight.vim'
Plug 'qpkorr/vim-bufkill'
Plug 'scrooloose/nerdtree'
Plug 'tpope/vim-commentary'
Plug 'tpope/vim-endwise'
Plug 'tpope/vim-eunuch'
Plug 'tpope/vim-fugitive'
Plug 'tpope/vim-repeat'
Plug 'tpope/vim-sleuth'
Plug 'tpope/vim-surround'
Plug 'tpope/vim-unimpaired'
Plug 'wellle/targets.vim'

" Syntax (polyglot being the most important)
Plug 'alampros/vim-styled-jsx'
Plug 'sheerun/vim-polyglot'

" Colors
Plug 'tomasr/molokai'
Plug 'lancewilhelm/horizon-extended.nvim'

call plug#end()

" ----------------------------------------------------------------------------
" OPTIONS
" ----------------------------------------------------------------------------

colorscheme horizon-extended

" ----------------------------------------------------------------------------
" KEY MAPPINGS
" ----------------------------------------------------------------------------

nmap \e :NERDTreeToggle<CR>
nmap \l :setlocal number! relativenumber!<CR>:setlocal number?<CR>
nmap \m :set expandtab tabstop=2 shiftwidth=2 softtabstop=2<CR>
nmap \o :set paste!<CR>:set paste?<CR>
nmap \q :nohlsearch<CR>
nmap \s :setlocal invspell<CR>
nmap \t :set expandtab tabstop=4 shiftwidth=4 softtabstop=4<CR>
nmap \u :setlocal list!<CR>:setlocal list?<CR>
nmap \w :setlocal wrap!<CR>:setlocal wrap?<CR>

" Turn off linewise keys. Normally, the `j' and `k' keys move the cursor down
" one entire line. with line wrapping on, this can cause the cursor to
" actually skip a few lines on the screen because it's moving from line N to
" line N+1 in the file. I want this to act more visually -- I want `down' to
" mean the next line on the screen
nmap j gj
nmap k gk

" Marks should go to the column, not just the line. Why isn't this the default?
nnoremap ' `

" You don't know what you're missing if you don't use this.
nnoremap <C-e> :e#<CR>

" Move between open buffers.
nmap <C-n> :bnext<CR>
nmap <C-p> :bprev<CR>

" Emacs-like bindings in normal mode
nmap <C-x>0 <C-w>c
nmap <C-x>1 <C-w>o
nmap <C-x>1 <C-w>s
nmap <C-x>1 <C-w>v
nmap <C-x>o <C-w><C-w>
nmap <M-o>  <C-w><C-w>

" Emacs-like bindings in insert mode
imap <C-e> <C-o>$
imap <C-a> <C-o>0

" Emacs-like bindings in the command line from `:h emacs-keys`
cnoremap <C-a>  <Home>
cnoremap <C-b>  <Left>
cnoremap <C-f>  <Right>
cnoremap <C-d>  <Del>
cnoremap <C-e>  <End>
cnoremap <M-b>  <S-Left>
cnoremap <M-f>  <S-Right>
cnoremap <M-d>  <S-right><Delete>
cnoremap <Esc>b <S-Left>
cnoremap <Esc>f <S-Right>
cnoremap <Esc>d <S-right><Delete>
cnoremap <C-g>  <C-c>

" Super fast window movement shortcuts
nmap <C-j> <C-W>j
nmap <C-k> <C-W>k
nmap <C-h> <C-W>h
nmap <C-l> <C-W>l

" Search for the word under the cursor in the current directory
nmap <M-k>    :Rg <C-R><C-W><CR>
nmap <Esc>k   :Rg <C-R><C-W><CR>

" Alt-W to delete a buffer and remove it from the list but keep the window via bufkill.vim
nmap <Esc>w :BD<CR>
nmap <M-w>  :BD<CR>

" Quickly fix spelling errors choosing the first result
nmap <Leader>z z=1<CR><CR>

" Make the cursor stay on the same line when window switching
function! KeepCurrentLine(motion)
  let theLine = line('.')
  let theCol = col('.')
  exec 'wincmd ' . a:motion
  if &diff
    call cursor(theLine, theCol)
  endif
endfunction
nnoremap <C-w>h :silent call KeepCurrentLine('h')<CR>
nnoremap <C-w>l :silent call KeepCurrentLine('l')<CR>

" ----------------------------------------------------------------------------
" CUSTOM COMMANDS AND FUNCTIONS
" ----------------------------------------------------------------------------

" I always hit ":W" instead of ":w" because I linger on the shift key...
command! Q q
command! W w

" Trim spaces at EOL and retab. I run `:CLEAN` a lot to clean up files.
command! TEOL %s/\s\+$//
command! CLEAN retab | TEOL

" ----------------------------------------------------------------------------
" PLUGIN SETTINGS
" ----------------------------------------------------------------------------

" FZF (replaces Ctrl-P, FuzzyFinder and Command-T)
nmap ; :Files<CR>
nmap <Leader>r :Tags<CR>
nmap <Leader>b :Buffers<CR>
nmap <Leader>t :Files<CR>
nmap <Leader>a :Rg!<CR>
nmap <Leader>c :Colors<CR>
" let $FZF_DEFAULT_COMMAND = 'rg --files --follow -g "!{.git,node_modules}/*" 2>/dev/null'
" command! -bang -nargs=* Rg
"   \ call fzf#vim#grep(
"   \   'rg --column --line-number --no-heading --color=always --smart-case -g "!{*.lock,*-lock.json}" '.shellescape(<q-args>), 1,
"   \   <bang>0 ? fzf#vim#with_preview('up:40%')
"   \           : fzf#vim#with_preview('right:50%:hidden', '?'),
"   \   <bang>0)

" " FZF color scheme updater from https://github.com/junegunn/fzf.vim/issues/59
" function! s:update_fzf_colors()
"   let rules =
"   \ { 'fg':      [['Normal',       'fg']],
"     \ 'bg':      [['Normal',       'bg']],
"     \ 'hl':      [['String',       'fg']],
"     \ 'fg+':     [['CursorColumn', 'fg'], ['Normal', 'fg']],
"     \ 'bg+':     [['CursorColumn', 'bg']],
"     \ 'hl+':     [['String',       'fg']],
"     \ 'info':    [['PreProc',      'fg']],
"     \ 'prompt':  [['Conditional',  'fg']],
"     \ 'pointer': [['Exception',    'fg']],
"     \ 'marker':  [['Keyword',      'fg']],
"     \ 'spinner': [['Label',        'fg']],
"     \ 'header':  [['Comment',      'fg']] }
"   let cols = []
"   for [name, pairs] in items(rules)
"     for pair in pairs
"       let code = synIDattr(synIDtrans(hlID(pair[0])), pair[1])
"       if !empty(name) && code != ''
"         call add(cols, name.':'.code)
"         break
"       endif
"     endfor
"   endfor
"   let s:orig_fzf_default_opts = get(s:, 'orig_fzf_default_opts', $FZF_DEFAULT_OPTS)
"   let $FZF_DEFAULT_OPTS = s:orig_fzf_default_opts .
"         \ (empty(cols) ? '' : (' --color='.join(cols, ',')))
" endfunction

" augroup _fzf
"   autocmd!
"   autocmd VimEnter,ColorScheme * call <sid>update_fzf_colors()
" augroup END

" More space with NERDTree
let g:NERDTreeMinimalUI = 1
let g:NERDTreeMarkBookmarks = 0
let g:NERDTreeAutoDeleteBuffer = 1
let g:NERDTreeStatusLine = -1

" GitGutter styling to use · instead of +/-
let g:gitgutter_sign_added = '▎'
let g:gitgutter_sign_modified = '▎'
let g:gitgutter_sign_removed = '▎'
let g:gitgutter_sign_modified_removed = '•'
nmap ]g :GitGutterNextHunk<CR>
nmap [g :GitGutterPrevHunk<CR>
augroup VimDiff
  autocmd!
  autocmd VimEnter,FilterWritePre * if &diff | GitGutterDisable | endif
augroup END

" SuperTab
let g:SuperTabLongestEnhanced = 1
let g:SuperTabLongestHighlight = 1
au Filetype typescript let b:SuperTabDefaultCompletionType = "<C-x><C-o>"

" csv.vim
let g:csv_delim_test = ',|'
let g:csv_no_conceal = 1
highlight link CSVDelimiter Delimiter
highlight link CSVColumnHeaderEven Type
highlight link CSVColumnHeaderOdd Type

" Use incsearch.vim to highlight as I search
map /  <Plug>(incsearch-forward)
map ?  <Plug>(incsearch-backward)
map g/ <Plug>(incsearch-stay)

" Highlight YAML frontmatter in Markdown files
let g:vim_markdown_frontmatter = 1

" Goyo and Limelight integration
autocmd! User GoyoEnter Limelight
autocmd! User GoyoLeave Limelight!

" ----------------------------------------------------------------------------
" FILE TYPE TRIGGERS
" ----------------------------------------------------------------------------

" Reset all autocommands
augroup vimrc
autocmd!

au BufNewFile,BufRead *.cson    set ft=coffee
au BufNewFile,BufRead *.glsl    setf glsl
au BufNewFile,BufRead *.gyp     set ft=python
au BufNewFile,BufRead *.html    setlocal nocindent smartindent
au BufNewFile,BufRead *.i7x     setf inform7
au BufNewFile,BufRead *.ini     setf conf
au BufNewFile,BufRead *.input   setf gnuplot
au BufNewFile,BufRead *.journal setlocal tw=0 ts=4 sw=4 et
au BufNewFile,BufRead *.json    set ft=json tw=0
au BufNewFile,BufRead *.less    setlocal ft=less nocindent smartindent
au BufNewFile,BufRead *.lkml    setf yaml
au BufNewFile,BufRead *.md      setlocal ft=markdown nolist spell
au BufNewFile,BufRead *.md,*.markdown setlocal foldlevel=999 tw=0 nocin
au BufNewFile,BufRead *.ni,*.i7x      setlocal ft=inform7 fdm=manual nolist ts=2 sw=2 noet spell
au BufNewFile,BufRead *.plist   setf xml
au BufNewFile,BufRead *.rb      setlocal noai
au BufNewFile,BufRead *.rxml    setf ruby
au BufNewFile,BufRead *.sass    setf sass
au BufNewFile,BufRead *.ttml    setf xml
au BufNewFile,BufRead *.vert,*.frag set ft=glsl
au BufNewFile,BufRead *.xml     setlocal ft=xml ts=2 sw=2 et
au BufNewFile,BufRead *.zone    setlocal nolist ts=4 sw=4 noet
au BufNewFile,BufRead *.zsh     setf zsh
au BufNewFile,BufRead *.ovpn    setf openvpn
au BufNewFile,BufRead *templates/*.html setf htmldjango
au BufNewFile,BufRead .conkyrc set ft=lua
au BufNewFile,BufRead .git/config setlocal ft=gitconfig nolist ts=4 sw=4 noet
au BufNewFile,BufRead .gitconfig* setlocal ft=gitconfig nolist ts=4 sw=4 noet
au BufNewFile,BufRead postgres*.conf setlocal nolist ts=8 sw=8 noet
au BufNewFile,BufRead .vimlocal,.gvimlocal setf vim
au BufNewFile,BufRead .nvimlocal setf vim
au BufNewFile,BufRead .zshlocal setf zsh
au BufNewFile,BufRead /tmp/crontab* setf crontab
au BufNewFile,BufRead COMMIT_EDITMSG setlocal nolist nonumber
au BufNewFile,BufRead Makefile setlocal nolist
au BufNewFile,BufRead dist/* set ft=text
au BufNewFile,BufRead Caddyfile setlocal nolist ts=4 sw=4 noet
au BufNewFile,BufRead *crontabs* setf crontab

au FileType gitcommit setlocal nolist ts=4 sts=4 sw=4 noet
au FileType inform7 setlocal nolist tw=0 ts=4 sw=4 noet foldlevel=999
au FileType json setlocal conceallevel=0 foldmethod=syntax foldlevel=999
au FileType make setlocal nolist ts=4 sts=4 sw=4 noet
au FileType markdown syn sync fromstart
au Filetype gitcommit setlocal tw=80
au Filetype csv setlocal nocursorline

augroup END

" ----------------------------------------------------------------------------
" GUI-SPECIFIC SETTINGS (for Neovide)
" ----------------------------------------------------------------------------

if has('gui_running')
  " Font (99% as good as PragmataPro, 100% free! Ligatures are kinda cute, too.)
  set guifont=Iosevka-Term-Medium:h16

  " Window size
  set columns=80 lines=50

  " GUI options
  set errorbells
  set guicursor+=a:blinkon0
  set guioptions=igm
  set mousemodel=popup
  set cursorline
  set nonumber
  set scrolloff=15
  set sidescrolloff=6
  set visualbell

  " Title
  set title titlestring=Neovide\ \|\ %{substitute(getcwd(),\ $HOME,\ '~',\ '')}

  " Gutter color default is annoying. Make it blend in.
  augroup _gvimrc
    autocmd!
    autocmd ColorScheme * hi SignColumn guibg=NONE

    " FZF uses ctermfg for some colors. Ew.
    autocmd ColorScheme * hi Number ctermfg=7
    autocmd ColorScheme * hi Conditional ctermfg=7
    autocmd ColorScheme * hi Special ctermfg=7
  augroup END
endif

" ----------------------------------------------------------------------------
" HOST-SPECIFIC CONFIG
" ----------------------------------------------------------------------------

" Now load specifics to this host
if filereadable(expand("~/.config/nvim/nvimlocal.vim"))
  source ~/.config/nvim/nvimlocal.vim
endif

" Some plugin seems to search for something at startup, so this fixes that.
silent! nohlsearch
