" Neovim Configuration
" Updated with modern plugins - 2025

" ----------------------------------------------------------------------------
" PLUGIN MANAGER: lazy.nvim
" ----------------------------------------------------------------------------

" Leader key (must be set before lazy so mappings are correct)
let mapleader = ','
let maplocalleader = ','

" Bootstrap and setup lazy.nvim
lua << LAZY_EOF
local lazypath = vim.fn.stdpath("data") .. "/lazy/lazy.nvim"
if not vim.loop.fs_stat(lazypath) then
  vim.fn.system({
    "git",
    "clone",
    "--filter=blob:none",
    "https://github.com/folke/lazy.nvim.git",
    "--branch=stable",
    lazypath,
  })
end
vim.opt.rtp:prepend(lazypath)

require("lazy").setup({
  -- File Explorer (replaces NERDTree)
  {
    "nvim-tree/nvim-tree.lua",
    dependencies = { "nvim-tree/nvim-web-devicons" },
    cmd = { "NvimTreeToggle", "NvimTreeFindFile" },
  },

  -- Fuzzy Finder (replaces fzf.vim)
  {
    "nvim-telescope/telescope.nvim",
    dependencies = {
      "nvim-lua/plenary.nvim",
      { "nvim-telescope/telescope-fzf-native.nvim", build = "make" },
    },
    cmd = "Telescope",
  },

  -- Statusline (replaces powerline/lightline)
  "nvim-lualine/lualine.nvim",

  -- Git (replaces vim-gitgutter)
  "lewis6991/gitsigns.nvim",
  "tpope/vim-fugitive",

  -- Syntax Highlighting (replaces vim-polyglot)
  {
    "nvim-treesitter/nvim-treesitter",
    build = ":TSUpdate",
  },
  {
    "windwp/nvim-ts-autotag",
    event = "VeryLazy",
    opts = {},
  },

  -- LSP and Completion (replaces supertab)
  "neovim/nvim-lspconfig",
  "williamboman/mason.nvim",
  "williamboman/mason-lspconfig.nvim",
  {
    "hrsh7th/nvim-cmp",
    dependencies = {
      "hrsh7th/cmp-nvim-lsp",
      "hrsh7th/cmp-buffer",
      "hrsh7th/cmp-path",
      "hrsh7th/cmp-cmdline",
      "hrsh7th/cmp-nvim-lua",
      "L3MON4D3/LuaSnip",
      "saadparwaiz1/cmp_luasnip",
    },
    event = "InsertEnter",
  },

  -- Formatting
  "stevearc/conform.nvim",

  -- Keybinding discovery
  "folke/which-key.nvim",

  -- Auto-pairs
  {
    "windwp/nvim-autopairs",
    event = "InsertEnter",
    dependencies = { "hrsh7th/nvim-cmp" },
  },

  -- Search improvements (replaces incsearch.vim)
  "kevinhwang91/nvim-hlslens",

  -- Still excellent plugins (keep these)
  "tpope/vim-commentary",
  "tpope/vim-endwise",
  "tpope/vim-eunuch",
  "tpope/vim-repeat",
  "tpope/vim-sleuth",
  "tpope/vim-surround",
  "tpope/vim-unimpaired",
  "qpkorr/vim-bufkill",
  "wellle/targets.vim",

  -- Focus/writing mode
  "junegunn/goyo.vim",
  "junegunn/limelight.vim",

  -- Colors
  "tomasr/molokai",
  "lancewilhelm/horizon-extended.nvim",
}, {
  install = {
    colorscheme = { "horizon-extended", "molokai" },
  },
  checker = {
    enabled = true,
    notify = false,
  },
  change_detection = {
    notify = false,
  },
})
LAZY_EOF

" ----------------------------------------------------------------------------
" OPTIONS
" ----------------------------------------------------------------------------


" Enable true color support (required for modern colorschemes)
if has('termguicolors')
  set termguicolors
endif

" Line numbers
set number                  " Show line numbers
set relativenumber          " Show relative line numbers by default

" File handling
set autoread                " Reload files changed outside vim
set autowrite               " Write on :next/:prev/^Z
set directory-=.            " Don't store temp files in cwd
set fileformats=unix,dos,mac  " Prefer Unix line endings
set nobackup                " No backup files
set nowritebackup           " No backup while editing
set suffixes+=.pyc          " Ignore .pyc in tab completion

" Indentation
set expandtab               " Use spaces instead of tabs
set shiftround              " Round indent to multiple of shiftwidth
set shiftwidth=2            " Default indent size
set softtabstop=2           " Spaces feel like tabs
set tabstop=2               " Tab width
set copyindent              " Copy indent structure from previous line

" Text formatting
set formatoptions=tcqn1     " t=autowrap text, c=autowrap comments, q=gq formats comments, n=autowrap lists, 1=break before single-letter words
set textwidth=0             " No automatic line wrapping
set linebreak               " Break long lines by word, not char
set showbreak=              " Show wrapped line prefix

" Search
set ignorecase              " Case insensitive search
set smartcase               " Case sensitive when search contains uppercase
set infercase               " Completion recognizes capitalization

" Display
set list                    " Show whitespace (see listchars)
set listchars=tab:»\ ,extends:›,precedes:‹,nbsp:·,trail:·  " Unicode chars for whitespace
set matchtime=2             " Tenths of second to highlight matching paren
set showmatch               " Highlight matching braces/parens
set scroll=4                " Lines to scroll with ^U/^D
set scrolloff=15            " Keep cursor away from top/bottom
set sidescrolloff=3         " Keep cursor away from left/right edges
set fillchars=vert:│,stl:\ ,stlnc:\ ,fold:-,diff:│  " Unicode chars for UI elements (│ for window borders)

" Folding
silent! set foldmethod=marker  " Use braces by default for folding
set commentstring=\ \ #%s   " Comment string for folds

" Completion
set wildmenu                " Show completion menu
set wildmode=list:longest,full  " List all, then complete
set wildignore=*.class,*.o,*~,*.pyc,.git,node_modules  " Ignore in tab completion

" History
set history=200             " Command history lines

" Security
set nomodeline              " Disable modelines (security)

" Visual/audio
set visualbell              " Visual bell instead of beep
set t_vb=                   " No terminal bell

" Session
set sessionoptions-=options  " Don't save runtimepath in sessions

" Swapfile messages
set shortmess+=A            " Don't prompt when swapfile exists

" Essential for filetype plugins
filetype plugin indent on

" Colorscheme will be set in Lua config section after plugins load

" ----------------------------------------------------------------------------
" KEY MAPPINGS
" ----------------------------------------------------------------------------

" File explorer (replaces NERDTree)
nmap \e <cmd>lua pcall(function() require('nvim-tree.api').tree.toggle() end)<CR>
nmap \F <cmd>lua pcall(function() require('nvim-tree.api').tree.find_file({ open = true, focus = true }) end)<CR>

" Format options
nmap \A :set formatoptions+=a<CR>:echo "autowrap enabled"<CR>
nmap \a :set formatoptions-=a<CR>:echo "autowrap disabled"<CR>
nmap \b :set nocin tw=80<CR>:set formatoptions+=a<CR>

" Tab settings
nmap \M :set noexpandtab tabstop=8 softtabstop=4 shiftwidth=4<CR>
nmap \T :set expandtab tabstop=8 shiftwidth=8 softtabstop=4<CR>
nmap \m :set expandtab tabstop=2 shiftwidth=2 softtabstop=2<CR>
nmap \t :set expandtab tabstop=4 shiftwidth=4 softtabstop=4<CR>

" Other toggles
nmap \l :setlocal number! relativenumber!<CR>:setlocal number?<CR>
nmap \o :set paste!<CR>:set paste?<CR>
nmap \q :nohlsearch<CR>
nmap \s :setlocal invspell<CR>
nmap \u :setlocal list!<CR>:setlocal list?<CR>
nmap \w :setlocal wrap!<CR>:setlocal wrap?<CR>
nmap \x :cclose<CR>
nmap \z :w<CR>:!open %<CR><CR>

" Git (replaces GitGutter toggle)
nmap \g <cmd>lua pcall(function() require('gitsigns').toggle() end)<CR>

" Sorting
nmap \i vip:sort<CR>

" Writing mode (replaces ProseMode)
nmap \p <cmd>Goyo<CR>
nmap \W mt:Goyo<CR>'tzz

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
nmap <C-x>2 <C-w>s
nmap <C-x>3 <C-w>v
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

" Resize panes when window/terminal gets resized
autocmd VimResized * :wincmd =

" Search for the word under the cursor (using Telescope)
nmap <M-k>    <cmd>Telescope grep_string<CR>
nmap <Esc>k   <cmd>Telescope grep_string<CR>

" Alt-W to delete a buffer and remove it from the list but keep the window via bufkill.vim
nmap <Esc>w :BD<CR>
nmap <M-w>  :BD<CR>

" Quickly fix spelling errors choosing the first result
nmap <Leader>z z=1<CR><CR>

" Show which-key popup
nmap <Leader><Leader> <cmd>WhichKey<CR>

" These are things that I mistype and want ignored.
nmap Q  <silent>
nmap q: <silent>
nmap K  <silent>

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

" Close all buffers except this one
command! BufCloseOthers %bd|e#

" Typing `$c` on the command line expands to `:e` + the current path
cnoremap $c e <C-\>eCurrentFileDir()<CR>
function! CurrentFileDir()
   return "e " . expand("%:p:h") . "/"
endfunction


" ----------------------------------------------------------------------------
" PLUGIN SETTINGS
" ----------------------------------------------------------------------------

" Telescope (replaces FZF)
nmap ; <cmd>Telescope find_files<CR>
nmap <Leader>r <cmd>Telescope tags<CR>
nmap <Leader>t <cmd>Telescope find_files<CR>
nmap <Leader>a <cmd>Telescope live_grep<CR>
nmap <Leader>c <cmd>Telescope colorscheme<CR>
nmap <Leader>gh <cmd>Telescope git_commits<CR>
nmap <Leader>gb <cmd>Telescope git_branches<CR>

" Git signs navigation (replaces GitGutter)
nmap ]g <cmd>lua pcall(function() require('gitsigns').next_hunk() end)<CR>
nmap [g <cmd>lua pcall(function() require('gitsigns').prev_hunk() end)<CR>

" Diagnostic navigation
nmap ]d <cmd>lua vim.diagnostic.goto_next()<CR>
nmap [d <cmd>lua vim.diagnostic.goto_prev()<CR>
nmap <Leader>e <cmd>lua vim.diagnostic.open_float()<CR>

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

" Disable gitsigns in diff mode
au VimEnter,FilterWritePre * if &diff | lua pcall(function() require('gitsigns').toggle_signs(false) end) | endif

" Goyo and Limelight integration
autocmd! User GoyoEnter Limelight
autocmd! User GoyoLeave Limelight!

" Custom mode for distraction-free editing (replaces ProseMode)
function! ProseMode()
  call goyo#execute(0, [])
  set spell noci nosi noai nolist noshowmode noshowcmd
  set complete+=s
  colorscheme horizon-extended
endfunction
command! ProseMode call ProseMode()

augroup END

" ----------------------------------------------------------------------------
" ABBREVIATIONS
" ----------------------------------------------------------------------------

" I can't spell :(
abbr conosle console
abbr comopnent component

" Debugging helpers
autocmd BufEnter *.py iabbr xxx print('XXX
autocmd BufEnter *.py iabbr yyy print('YYY
autocmd BufEnter *.py iabbr zzz print('ZZZ
autocmd BufEnter *.coffee iabbr xxx console.log 'XXX',
autocmd BufEnter *.coffee iabbr yyy console.log 'YYY',
autocmd BufEnter *.coffee iabbr zzz console.log 'ZZZ',
autocmd BufEnter *.js iabbr xxx console.log('XXX',
autocmd BufEnter *.js iabbr yyy console.log('YYY',
autocmd BufEnter *.js iabbr zzz console.log('ZZZ',
autocmd BufEnter *.ts iabbr xxx console.log('XXX',
autocmd BufEnter *.ts iabbr yyy console.log('YYY',
autocmd BufEnter *.ts iabbr zzz console.log('ZZZ',
autocmd BufEnter *.rb iabbr xxx puts "XXX
autocmd BufEnter *.rb iabbr yyy puts "YYY
autocmd BufEnter *.rb iabbr zzz puts "ZZZ
autocmd BufEnter *.rb iabbr ppp require 'pp'; pp

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

    " Telescope uses ctermfg for some colors. Ew.
    autocmd ColorScheme * hi Number ctermfg=7
    autocmd ColorScheme * hi Conditional ctermfg=7
    autocmd ColorScheme * hi Special ctermfg=7
  augroup END
endif

" Make the damned tildes less visible
highlight link EndOfBuffer Comment

" Make menu selections visible
highlight PmenuSel ctermfg=black ctermbg=magenta

" ----------------------------------------------------------------------------
" HOST-SPECIFIC CONFIG
" ----------------------------------------------------------------------------

" Now load specifics to this host
if filereadable(expand("~/.config/nvim/nvimlocal.vim"))
  source ~/.config/nvim/nvimlocal.vim
endif

" Some plugin seems to search for something at startup, so this fixes that.
silent! nohlsearch

" ----------------------------------------------------------------------------
" LUA CONFIGURATION (for modern plugins)
" ----------------------------------------------------------------------------

lua << EOF
-- Set colorscheme (after plugins are loaded)
-- Try to setup and load horizon-extended
local function setup_colorscheme()
  local ok, horizon = pcall(require, 'horizon-extended')
  if ok and horizon then
    -- Setup with custom options
    horizon.setup({
      style = 'neo',
      transparent = false,
      terminal_colors = true,
      enable_italics = true,
      show_end_of_buffer = false,
      underline = false,
      undercurl = true,
    })
    vim.cmd('colorscheme horizon-extended')
    return true
  else
    -- Fallback to molokai
    pcall(function()
      vim.cmd('colorscheme molokai')
    end)
    return false
  end
end

-- Set colorscheme immediately
setup_colorscheme()

-- Also set it on VimEnter in case plugins weren't loaded yet
vim.api.nvim_create_autocmd('VimEnter', {
  callback = function()
    if vim.g.colors_name ~= 'horizon-extended' then
      setup_colorscheme()
    end
  end,
})

-- Safely load plugins (only if they're installed)
-- nvim-tree (replaces NERDTree)
pcall(function()
  require('nvim-tree').setup({
    view = {
      width = 30,
      side = 'left',
    },
    renderer = {
      icons = {
        show = {
          git = true,
        },
      },
    },
    filters = {
      dotfiles = false,
    },
  })
end)

-- Telescope (replaces fzf.vim)
pcall(function()
  require('telescope').setup({
    defaults = {
      file_ignore_patterns = { '^.git/', '^node_modules/', '^.DS_Store' },
    },
    pickers = {
      find_files = {
        find_command = { 'rg', '--files', '--follow', '--hidden', '-g', '!{.git,node_modules}/*' },
      },
    },
  })
  pcall(require('telescope').load_extension, 'fzf')
end)

-- lualine (statusline)
pcall(function()
  require('lualine').setup({
    options = {
      theme = 'auto',
      component_separators = { left = '|', right = '|' },
      section_separators = { left = '', right = '' },
      disabled_filetypes = {
        statusline = { 'NvimTree' },
      },
    },
    sections = {
      lualine_a = { 'mode' },
      lualine_b = { 'branch', 'diff', 'diagnostics' },
      lualine_c = { 'filename' },
      lualine_x = { 'encoding', 'fileformat', 'filetype' },
      lualine_y = { 'progress' },
      lualine_z = { 'location' },
    },
  })
end)

-- gitsigns (replaces vim-gitgutter)
pcall(function()
  require('gitsigns').setup({
    signs = {
      add = { text = '▎' },
      change = { text = '▎' },
      delete = { text = '▎' },
      topdelete = { text = '▎' },
      changedelete = { text = '•' },
    },
  })
end)

-- treesitter (replaces vim-polyglot)
pcall(function()
  require('nvim-treesitter.configs').setup({
    ensure_installed = {
      'lua', 'vim', 'vimdoc', 'query',
      'javascript', 'typescript', 'html', 'css', 'json',
      'markdown', 'python', 'bash', 'yaml', 'ruby',
      'xml', 'glsl', 'coffee', 'sass', 'less',
    },
    sync_install = false,
    auto_install = true,
    highlight = {
      enable = true,
    },
    indent = {
      enable = true,
    },
  })
end)

-- nvim-ts-autotag (replaces closetag.vim - auto-closes HTML/XML/JSX tags)
pcall(function()
  require('nvim-ts-autotag').setup({
    filetypes = {
      'html',
      'javascript',
      'typescript',
      'javascriptreact',
      'typescriptreact',
      'svelte',
      'vue',
      'tsx',
      'jsx',
      'rescript',
      'xml',
      'php',
      'markdown',
      'astro',
      'glimmer',
      'handlebars',
      'hbs',
    },
    skip_tag_on_closing = false, -- Don't skip closing tag when typing >
  })
end)

-- hlslens (replaces incsearch.vim)
pcall(function()
  require('hlslens').setup({
    calm_down = true,
    nearest_only = true,
    nearest_float_when = 'always',
  })
  
  -- Setup keybindings for hlslens
  local kopts = { noremap = true, silent = true }
  
  -- Use hlslens bindings that work with search
  vim.keymap.set({'n', 'x'}, 'n', [[<Cmd>execute('normal! ' . v:count1 . 'n')<CR><Cmd>lua require('hlslens').start()<CR>]], kopts)
  vim.keymap.set({'n', 'x'}, 'N', [[<Cmd>execute('normal! ' . v:count1 . 'N')<CR><Cmd>lua require('hlslens').start()<CR>]], kopts)
  vim.keymap.set('n', '*', [[*<Cmd>lua require('hlslens').start()<CR>]], kopts)
  vim.keymap.set('n', '#', [[#<Cmd>lua require('hlslens').start()<CR>]], kopts)
  vim.keymap.set('n', 'g*', [[g*<Cmd>lua require('hlslens').start()<CR>]], kopts)
  vim.keymap.set('n', 'g#', [[g#<Cmd>lua require('hlslens').start()<CR>]], kopts)
  
  -- Auto-start hlslens when searching
  vim.api.nvim_create_autocmd('CmdlineEnter', {
    pattern = {'/', '?'},
    callback = function()
      vim.schedule(function()
        require('hlslens').start()
      end)
    end,
  })
end)

-- Configure diagnostics
vim.diagnostic.config({
  virtual_text = {
    severity = { min = vim.diagnostic.severity.WARN },
    source = 'always',
    prefix = '●',
  },
  signs = true,
  underline = true,
  update_in_insert = false,
  severity_sort = true,
  float = {
    border = 'rounded',
    source = 'always',
    header = '',
    prefix = '',
  },
})

-- Define diagnostic signs
local signs = {
  { name = 'DiagnosticSignError', text = '●' },
  { name = 'DiagnosticSignWarn', text = '●' },
  { name = 'DiagnosticSignHint', text = '●' },
  { name = 'DiagnosticSignInfo', text = '●' },
}

for _, sign in ipairs(signs) do
  vim.fn.sign_define(sign.name, {
    texthl = sign.name,
    text = sign.text,
    numhl = '',
  })
end

-- Mason (LSP installer)
pcall(function()
  require('mason').setup()
  require('mason-lspconfig').setup({
    ensure_installed = { 'lua_ls', 'pyright', 'ts_ls' },
  })
end)

-- LSP setup with custom configurations (using Neovim 0.11+ vim.lsp.config API)
pcall(function()
  local capabilities = require('cmp_nvim_lsp').default_capabilities()

  -- Lua LSP with custom settings
  vim.lsp.config('lua_ls', {
    settings = {
      Lua = {
        runtime = { version = 'LuaJIT' },
        workspace = { checkThirdParty = false },
        telemetry = { enable = false },
      },
    },
    capabilities = capabilities,
  })
  vim.lsp.enable('lua_ls')

  -- Python LSP with custom settings
  vim.lsp.config('pyright', {
    settings = {
      python = {
        analysis = {
          autoSearchPaths = true,
          useLibraryCodeForTypes = true,
        },
      },
    },
    capabilities = capabilities,
  })
  vim.lsp.enable('pyright')

  -- TypeScript/JavaScript LSP
  vim.lsp.config('tsserver', {
    capabilities = capabilities,
    settings = {
      typescript = {
        inlayHints = {
          includeInlayParameterNameHints = 'all',
          includeInlayParameterNameHintsWhenArgumentMatchesName = false,
          includeInlayFunctionParameterTypeHints = true,
          includeInlayVariableTypeHints = true,
          includeInlayPropertyDeclarationTypeHints = true,
          includeInlayFunctionLikeReturnTypeHints = true,
          includeInlayEnumMemberValueHints = true,
        },
      },
      javascript = {
        inlayHints = {
          includeInlayParameterNameHints = 'all',
          includeInlayParameterNameHintsWhenArgumentMatchesName = false,
          includeInlayFunctionParameterTypeHints = true,
          includeInlayVariableTypeHints = true,
          includeInlayPropertyDeclarationTypeHints = true,
          includeInlayFunctionLikeReturnTypeHints = true,
          includeInlayEnumMemberValueHints = true,
        },
      },
    },
  })
  vim.lsp.enable('tsserver')

  -- LSP key mappings
  vim.api.nvim_create_autocmd('LspAttach', {
    group = vim.api.nvim_create_augroup('UserLspConfig', {}),
    callback = function(ev)
      local opts = { buffer = ev.buf, desc = 'LSP' }
      vim.keymap.set('n', 'gD', vim.lsp.buf.declaration, vim.tbl_extend('force', opts, { desc = 'Go to declaration' }))
      vim.keymap.set('n', 'gd', vim.lsp.buf.definition, vim.tbl_extend('force', opts, { desc = 'Go to definition' }))
      vim.keymap.set('n', 'K', vim.lsp.buf.hover, vim.tbl_extend('force', opts, { desc = 'Hover' }))
      vim.keymap.set('n', 'gi', vim.lsp.buf.implementation, vim.tbl_extend('force', opts, { desc = 'Go to implementation' }))
      vim.keymap.set('n', '<C-k>', vim.lsp.buf.signature_help, vim.tbl_extend('force', opts, { desc = 'Signature help' }))
      vim.keymap.set('n', '<space>wa', vim.lsp.buf.add_workspace_folder, vim.tbl_extend('force', opts, { desc = 'Add workspace folder' }))
      vim.keymap.set('n', '<space>wr', vim.lsp.buf.remove_workspace_folder, vim.tbl_extend('force', opts, { desc = 'Remove workspace folder' }))
      vim.keymap.set('n', '<space>wl', function()
        print(vim.inspect(vim.lsp.buf.list_workspace_folders()))
      end, vim.tbl_extend('force', opts, { desc = 'List workspace folders' }))
      vim.keymap.set('n', '<space>D', vim.lsp.buf.type_definition, vim.tbl_extend('force', opts, { desc = 'Type definition' }))
      vim.keymap.set('n', '<space>rn', vim.lsp.buf.rename, vim.tbl_extend('force', opts, { desc = 'Rename' }))
      vim.keymap.set({ 'n', 'v' }, '<space>ca', vim.lsp.buf.code_action, vim.tbl_extend('force', opts, { desc = 'Code action' }))
      vim.keymap.set('n', 'gr', vim.lsp.buf.references, vim.tbl_extend('force', opts, { desc = 'References' }))
      vim.keymap.set('n', '<leader>f', function()
        vim.lsp.buf.format({ async = true })
      end, vim.tbl_extend('force', opts, { desc = 'Format' }))
    end,
  })

  -- Format on save
  vim.api.nvim_create_autocmd('BufWritePre', {
    group = vim.api.nvim_create_augroup('FormatOnSave', {}),
    callback = function(ev)
      local client = vim.lsp.get_active_clients({ bufnr = ev.buf })[1]
      if client and client.supports_method('textDocument/formatting') then
        vim.lsp.buf.format({ async = false, bufnr = ev.buf })
      end
    end,
  })
end)

-- which-key (keybinding discovery)
pcall(function()
  require('which-key').setup({
    plugins = {
      marks = true,
      registers = true,
      spelling = {
        enabled = true,
      },
      presets = {
        operators = true,
        motions = true,
        text_objects = true,
        windows = true,
        nav = true,
        z = true,
        g = true,
      },
    },
    win = {
      border = 'rounded',
    },
  })
end)

-- nvim-autopairs
pcall(function()
  local npairs = require('nvim-autopairs')
  npairs.setup({
    check_ts = true,
    ts_config = {
      lua = { 'string' },
      javascript = { 'template_string' },
    },
  })

  -- Integrate with cmp
  local cmp_autopairs = require('nvim-autopairs.completion.cmp')
  local cmp = require('cmp')
  cmp.event:on('confirm_done', cmp_autopairs.on_confirm_done())
end)

-- conform.nvim (formatting)
pcall(function()
  require('conform').setup({
    formatters_by_ft = {
      lua = { 'stylua' },
      python = { 'black', 'isort' },
      javascript = { 'biome' },
      typescript = { 'biome' },
      javascriptreact = { 'biome' },
      typescriptreact = { 'biome' },
      json = { 'biome' },
      jsonc = { 'biome' },
      css = { 'biome' },
      yaml = { 'prettier' },
      markdown = { 'prettier' },
      html = { 'prettier' },
      scss = { 'prettier' },
      sh = { 'shfmt' },
    },
    formatters = {
      biome = {
        command = 'biome',
        args = {
          'format',
          '--stdin-file-path',
          '$FILENAME',
        },
      },
    },
    format_on_save = {
      timeout_ms = 500,
      lsp_fallback = true,
    },
  })

  -- Map <leader>f to conform formatting
  vim.keymap.set({ 'n', 'v' }, '<leader>f', function()
    require('conform').format({ async = true, lsp_fallback = true })
  end, { desc = 'Format buffer' })
end)

-- Completion (nvim-cmp)
pcall(function()
  local cmp = require('cmp')
  local luasnip = require('luasnip')

  cmp.setup({
    snippet = {
      expand = function(args)
        luasnip.lsp_expand(args.body)
      end,
    },
    mapping = cmp.mapping.preset.insert({
      ['<C-b>'] = cmp.mapping.scroll_docs(-4),
      ['<C-f>'] = cmp.mapping.scroll_docs(4),
      ['<C-Space>'] = cmp.mapping.complete(),
      ['<C-e>'] = cmp.mapping.abort(),
      ['<CR>'] = cmp.mapping.confirm({ select = true }),
      ['<Tab>'] = cmp.mapping(function(fallback)
        if cmp.visible() then
          cmp.select_next_item()
        elseif luasnip.expand_or_jumpable() then
          luasnip.expand_or_jump()
        else
          fallback()
        end
      end, { 'i', 's' }),
      ['<S-Tab>'] = cmp.mapping(function(fallback)
        if cmp.visible() then
          cmp.select_prev_item()
        elseif luasnip.jumpable(-1) then
          luasnip.jump(-1)
        else
          fallback()
        end
      end, { 'i', 's' }),
    }),
    sources = cmp.config.sources({
      { name = 'nvim_lsp' },
      { name = 'luasnip' },
    }, {
      { name = 'buffer' },
      { name = 'path' },
      { name = 'nvim_lua' },
    }),
  })

  -- Command line completion
  cmp.setup.cmdline({ '/', '?' }, {
    mapping = cmp.mapping.preset.cmdline(),
    sources = {
      { name = 'buffer' },
    },
  })

  cmp.setup.cmdline(':', {
    mapping = cmp.mapping.preset.cmdline(),
    sources = cmp.config.sources({
      { name = 'path' },
    }, {
      { name = 'cmdline' },
    }),
  })
end)
EOF
