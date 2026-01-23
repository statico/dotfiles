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
Plug 'itchyny/lightline.vim'
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

" Linting (referenced in vimrc but not in original update.sh)
Plug 'dense-analysis/ale'

call plug#end()

" ----------------------------------------------------------------------------
" BASIC SETTINGS
" ----------------------------------------------------------------------------

" Leader key
let mapleader = ','

" Enable line numbers
set number
set relativenumber

" Enable syntax highlighting
syntax on
filetype plugin indent on

" Search settings
set incsearch
set hlsearch
set ignorecase
set smartcase

" Indentation
set autoindent
set smartindent
set expandtab
set tabstop=2
set shiftwidth=2
set softtabstop=2

" Visual settings
set showcmd
set showmode
set cursorline
set scrolloff=5
set sidescrolloff=5

" Better splitting
set splitbelow
set splitright

" File handling
set hidden
set autoread
set encoding=utf-8

" Colors
set background=dark
let g:rehash256 = 1 " Something to do with Molokai?
colorscheme molokai

" ----------------------------------------------------------------------------
" GUI-SPECIFIC SETTINGS (for Neovide)
" ----------------------------------------------------------------------------

if has('gui_running')
  " Neovide-specific settings can go here
  " For example:
  " set guifont=Iosevka\ Term\ Medium:h16
endif

" ----------------------------------------------------------------------------
" KEY MAPPINGS
" ----------------------------------------------------------------------------

" Clear search highlighting
nnoremap <leader>q :nohlsearch<CR>

" Quick save
nnoremap <leader>w :w<CR>

" ----------------------------------------------------------------------------
" HOST-SPECIFIC CONFIG
" ----------------------------------------------------------------------------

" Load machine-specific config if it exists
if filereadable(expand("~/.config/nvim/nvimlocal.vim"))
  source ~/.config/nvim/nvimlocal.vim
endif
