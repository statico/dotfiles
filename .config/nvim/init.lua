-- Neovim Configuration
-- Migrated to Lua - 2025

--------------------------------------------------------------------------------
-- LEADER KEY (must be set before lazy)
--------------------------------------------------------------------------------

vim.g.mapleader = ','
vim.g.maplocalleader = ','

--------------------------------------------------------------------------------
-- PLUGIN MANAGER: lazy.nvim
--------------------------------------------------------------------------------

local lazypath = vim.fn.stdpath('data') .. '/lazy/lazy.nvim'
if not vim.uv.fs_stat(lazypath) then
  vim.fn.system({
    'git',
    'clone',
    '--filter=blob:none',
    'https://github.com/folke/lazy.nvim.git',
    '--branch=stable',
    lazypath,
  })
end
vim.opt.rtp:prepend(lazypath)

require('lazy').setup({
  -- File Explorer
  {
    'nvim-tree/nvim-tree.lua',
    dependencies = { 'nvim-tree/nvim-web-devicons' },
    cmd = { 'NvimTreeToggle', 'NvimTreeFindFile' },
  },

  -- Fuzzy Finder
  {
    'nvim-telescope/telescope.nvim',
    dependencies = {
      'nvim-lua/plenary.nvim',
      { 'nvim-telescope/telescope-fzf-native.nvim', build = 'make' },
    },
    cmd = 'Telescope',
  },

  -- Statusline
  'nvim-lualine/lualine.nvim',

  -- Git
  'lewis6991/gitsigns.nvim',
  'tpope/vim-fugitive',
  {
    'sindrets/diffview.nvim',
    cmd = { 'DiffviewOpen', 'DiffviewClose', 'DiffviewFileHistory' },
  },

  -- Syntax Highlighting
  {
    'nvim-treesitter/nvim-treesitter',
    build = ':TSUpdate',
  },
  {
    'windwp/nvim-ts-autotag',
    event = 'VeryLazy',
    opts = {},
  },

  -- LSP and Completion
  'neovim/nvim-lspconfig',
  'williamboman/mason.nvim',
  'williamboman/mason-lspconfig.nvim',
  {
    'hrsh7th/nvim-cmp',
    dependencies = {
      'hrsh7th/cmp-nvim-lsp',
      'hrsh7th/cmp-buffer',
      'hrsh7th/cmp-path',
      'hrsh7th/cmp-cmdline',
      'hrsh7th/cmp-nvim-lua',
      'L3MON4D3/LuaSnip',
      'saadparwaiz1/cmp_luasnip',
    },
    event = 'InsertEnter',
  },

  -- Formatting
  'stevearc/conform.nvim',

  -- Keybinding discovery
  'folke/which-key.nvim',

  -- Auto-pairs
  {
    'windwp/nvim-autopairs',
    event = 'InsertEnter',
    dependencies = { 'hrsh7th/nvim-cmp' },
  },

  -- Search improvements
  'kevinhwang91/nvim-hlslens',

  -- Excellent tpope plugins
  'tpope/vim-commentary',
  'tpope/vim-endwise',
  'tpope/vim-eunuch',
  'tpope/vim-repeat',
  'tpope/vim-sleuth',
  'tpope/vim-surround',
  'tpope/vim-unimpaired',
  'qpkorr/vim-bufkill',
  'wellle/targets.vim',

  -- Focus/writing mode
  {
    'folke/zen-mode.nvim',
    cmd = 'ZenMode',
  },
  {
    'folke/twilight.nvim',
    cmd = 'Twilight',
  },

  -- Colors
  'tomasr/molokai',
  'lancewilhelm/horizon-extended.nvim',
}, {
  install = {
    colorscheme = { 'molokai', 'horizon-extended' },
  },
  checker = {
    enabled = true,
    notify = false,
  },
  change_detection = {
    notify = false,
  },
})

--------------------------------------------------------------------------------
-- OPTIONS
--------------------------------------------------------------------------------

local opt = vim.opt

-- True color support
opt.termguicolors = true

-- Line numbers
opt.number = true
opt.relativenumber = true

-- File handling
opt.autoread = true
opt.autowrite = true
opt.directory:remove('.')
opt.fileformats = { 'unix', 'dos', 'mac' }
opt.backup = false
opt.writebackup = false
opt.suffixes:append('.pyc')

-- Indentation
opt.expandtab = true
opt.shiftround = true
opt.shiftwidth = 2
opt.softtabstop = 2
opt.tabstop = 2
opt.copyindent = true

-- Text formatting
opt.formatoptions = 'tcqn1'
opt.textwidth = 0
opt.linebreak = true
opt.showbreak = ''

-- Search
opt.ignorecase = true
opt.smartcase = true
opt.infercase = true

-- Display
opt.list = true
opt.listchars = { tab = '» ', extends = '›', precedes = '‹', nbsp = '·', trail = '·' }
opt.matchtime = 2
opt.showmatch = true
opt.scroll = 4
opt.scrolloff = 15
opt.sidescrolloff = 3
opt.fillchars = { vert = '│', stl = ' ', stlnc = ' ', fold = '-', diff = '│' }

-- Folding
opt.foldmethod = 'marker'
opt.commentstring = '  #%s'

-- Completion
opt.wildmenu = true
opt.wildmode = { 'list:longest', 'full' }
opt.wildignore = { '*.class', '*.o', '*~', '*.pyc', '.git', 'node_modules' }

-- History
opt.history = 200

-- Security
opt.modeline = false

-- Visual/audio
opt.visualbell = true
opt.errorbells = false

-- Session
opt.sessionoptions:remove('options')

-- Swapfile messages
opt.shortmess:append('A')

--------------------------------------------------------------------------------
-- FILETYPE DETECTION
--------------------------------------------------------------------------------

vim.filetype.add({
  extension = {
    mdx = 'markdown',
  },
})

--------------------------------------------------------------------------------
-- KEY MAPPINGS
--------------------------------------------------------------------------------

local map = vim.keymap.set

-- File explorer
map('n', '\\e', '<cmd>lua pcall(function() require("nvim-tree.api").tree.toggle() end)<CR>')
map('n', '\\F',
  '<cmd>lua pcall(function() require("nvim-tree.api").tree.find_file({ open = true, focus = true }) end)<CR>')

-- Format options
map('n', '\\A', ':set formatoptions+=a<CR>:echo "autowrap enabled"<CR>')
map('n', '\\a', ':set formatoptions-=a<CR>:echo "autowrap disabled"<CR>')
map('n', '\\b', ':set nocin tw=80<CR>:set formatoptions+=a<CR>')

-- Tab settings
map('n', '\\M', ':set noexpandtab tabstop=8 softtabstop=4 shiftwidth=4<CR>')
map('n', '\\T', ':set expandtab tabstop=8 shiftwidth=8 softtabstop=4<CR>')
map('n', '\\m', ':set expandtab tabstop=2 shiftwidth=2 softtabstop=2<CR>')
map('n', '\\t', ':set expandtab tabstop=4 shiftwidth=4 softtabstop=4<CR>')

-- Other toggles
map('n', '\\l', ':setlocal number! relativenumber!<CR>:setlocal number?<CR>')
map('n', '\\o', ':set paste!<CR>:set paste?<CR>')
map('n', '\\q', ':nohlsearch<CR>')
map('n', '\\s', ':setlocal invspell<CR>')
map('n', '\\u', ':setlocal list!<CR>:setlocal list?<CR>')
map('n', '\\w', ':setlocal wrap!<CR>:setlocal wrap?<CR>')
map('n', '\\x', ':cclose<CR>')
map('n', '\\z', ':w<CR>:!open %<CR><CR>')

-- Git toggle
map('n', '\\g', '<cmd>lua pcall(function() require("gitsigns").toggle() end)<CR>')

-- Diffview toggle (git changes sidebar)
map('n', '\\d', function()
  local lib = require('diffview.lib')
  if lib.get_current_view() then
    vim.cmd('DiffviewClose')
  else
    vim.cmd('DiffviewOpen')
  end
end)

-- Sorting
map('n', '\\i', 'vip:sort<CR>')

-- Writing mode
map('n', '\\p', '<cmd>ZenMode<CR>')
map('n', '\\W', '<cmd>Twilight<CR>')

-- Visual line movement (j/k move by screen line, not file line)
map('n', 'j', 'gj')
map('n', 'k', 'gk')

-- Marks go to column, not just line
map('n', "'", '`')

-- Switch to alternate buffer
map('n', '<C-e>', ':e#<CR>')

-- Move between buffers
map('n', '<C-n>', ':bnext<CR>')
map('n', '<C-p>', ':bprev<CR>')

-- Emacs-like window bindings
map('n', '<C-x>0', '<C-w>c')
map('n', '<C-x>1', '<C-w>o')
map('n', '<C-x>2', '<C-w>s')
map('n', '<C-x>3', '<C-w>v')
map('n', '<C-x>o', '<C-w><C-w>')
map('n', '<M-o>', '<C-w><C-w>')

-- Emacs-like bindings in insert mode
map('i', '<C-e>', '<C-o>$')
map('i', '<C-a>', '<C-o>0')

-- Emacs-like bindings in command line
map('c', '<C-a>', '<Home>')
map('c', '<C-b>', '<Left>')
map('c', '<C-f>', '<Right>')
map('c', '<C-d>', '<Del>')
map('c', '<C-e>', '<End>')
map('c', '<M-b>', '<S-Left>')
map('c', '<M-f>', '<S-Right>')
map('c', '<M-d>', '<S-Right><Delete>')
map('c', '<C-g>', '<C-c>')

-- Fast window movement
map('n', '<C-j>', '<C-W>j')
map('n', '<C-k>', '<C-W>k')
map('n', '<C-h>', '<C-W>h')
map('n', '<C-l>', '<C-W>l')

-- Search for word under cursor
map('n', '<M-k>', '<cmd>Telescope grep_string<CR>')
map('n', '<Esc>k', '<cmd>Telescope grep_string<CR>')

-- Delete buffer but keep window
map('n', '<Esc>w', ':BD<CR>')
map('n', '<M-w>', ':BD<CR>')

-- Quick spelling fix
map('n', '<Leader>z', 'z=1<CR><CR>')

-- Show which-key popup
map('n', '<Leader><Leader>', '<cmd>WhichKey<CR>')

-- Ignore common mistypes
map('n', 'Q', '<Nop>', { silent = true })
map('n', 'q:', '<Nop>', { silent = true })

-- Telescope mappings
map('n', ';', '<cmd>Telescope find_files<CR>')
map('n', '<Leader>r', '<cmd>Telescope tags<CR>')
map('n', '<Leader>t', '<cmd>Telescope find_files<CR>')
map('n', '<Leader>a', '<cmd>Telescope live_grep<CR>')
map('n', '<Leader>c', '<cmd>Telescope colorscheme<CR>')
map('n', '<Leader>gh', '<cmd>Telescope git_commits<CR>')
map('n', '<Leader>gb', '<cmd>Telescope git_branches<CR>')

-- Git signs navigation
map('n', ']g', '<cmd>lua pcall(function() require("gitsigns").next_hunk() end)<CR>')
map('n', '[g', '<cmd>lua pcall(function() require("gitsigns").prev_hunk() end)<CR>')

-- Diagnostic navigation
map('n', ']d', '<cmd>lua vim.diagnostic.goto_next()<CR>')
map('n', '[d', '<cmd>lua vim.diagnostic.goto_prev()<CR>')
map('n', '<Leader>e', '<cmd>lua vim.diagnostic.open_float()<CR>')

--------------------------------------------------------------------------------
-- COMMANDS
--------------------------------------------------------------------------------

vim.api.nvim_create_user_command('Q', 'q', {})
vim.api.nvim_create_user_command('W', 'w', {})
vim.api.nvim_create_user_command('TEOL', '%s/\\s\\+$//', {})
vim.api.nvim_create_user_command('CLEAN', 'retab | TEOL', {})
vim.api.nvim_create_user_command('BufCloseOthers', '%bd|e#', {})

-- $c in command line expands to :e + current path
vim.keymap.set('c', '$c', function()
  return 'e ' .. vim.fn.expand('%:p:h') .. '/'
end, { expr = true })

--------------------------------------------------------------------------------
-- AUTOCOMMANDS
--------------------------------------------------------------------------------

local augroup = vim.api.nvim_create_augroup('vimrc', { clear = true })

-- Resize panes when window gets resized
vim.api.nvim_create_autocmd('VimResized', {
  group = augroup,
  pattern = '*',
  command = 'wincmd =',
})

-- Filetype-specific settings (only keeping essential ones not handled by treesitter/sleuth)
vim.api.nvim_create_autocmd({ 'BufNewFile', 'BufRead' }, {
  group = augroup,
  pattern = '*.md',
  callback = function()
    vim.opt_local.spell = true
    vim.opt_local.list = false
  end,
})

vim.api.nvim_create_autocmd({ 'BufNewFile', 'BufRead' }, {
  group = augroup,
  pattern = 'COMMIT_EDITMSG',
  callback = function()
    vim.opt_local.list = false
    vim.opt_local.number = false
    vim.opt_local.textwidth = 80
  end,
})

vim.api.nvim_create_autocmd({ 'BufNewFile', 'BufRead' }, {
  group = augroup,
  pattern = { '.git/config', '.gitconfig*' },
  callback = function()
    vim.opt_local.list = false
    vim.opt_local.tabstop = 4
    vim.opt_local.shiftwidth = 4
    vim.opt_local.expandtab = false
  end,
})

vim.api.nvim_create_autocmd({ 'BufNewFile', 'BufRead' }, {
  group = augroup,
  pattern = 'Makefile',
  callback = function()
    vim.opt_local.list = false
  end,
})

-- Disable gitsigns in diff mode
vim.api.nvim_create_autocmd({ 'VimEnter', 'FilterWritePre' }, {
  group = augroup,
  pattern = '*',
  callback = function()
    if vim.o.diff then
      pcall(function() require('gitsigns').toggle_signs(false) end)
    end
  end,
})

-- Zen mode (with twilight integration)
pcall(function()
  require('zen-mode').setup({
    on_open = function()
      vim.opt.scrolloff = 15
      vim.wo.number = false
      vim.wo.relativenumber = false
      vim.wo.signcolumn = 'no'
    end,
    on_close = function()
      vim.wo.number = true
      vim.wo.relativenumber = true
      vim.wo.signcolumn = 'yes'
    end,
    plugins = {
      twilight = { enabled = false },
      gitsigns = { enabled = false },
    },
  })
end)

-- Debugging abbreviations
vim.api.nvim_create_autocmd('BufEnter', {
  group = augroup,
  pattern = '*.py',
  callback = function()
    vim.cmd([[iabbrev <buffer> xxx print('XXX]])
    vim.cmd([[iabbrev <buffer> yyy print('YYY]])
    vim.cmd([[iabbrev <buffer> zzz print('ZZZ]])
  end,
})

vim.api.nvim_create_autocmd('BufEnter', {
  group = augroup,
  pattern = { '*.js', '*.ts' },
  callback = function()
    vim.cmd([[iabbrev <buffer> xxx console.log('XXX',]])
    vim.cmd([[iabbrev <buffer> yyy console.log('YYY',]])
    vim.cmd([[iabbrev <buffer> zzz console.log('ZZZ',]])
  end,
})

vim.api.nvim_create_autocmd('BufEnter', {
  group = augroup,
  pattern = '*.rb',
  callback = function()
    vim.cmd([[iabbrev <buffer> xxx puts "XXX]])
    vim.cmd([[iabbrev <buffer> yyy puts "YYY]])
    vim.cmd([[iabbrev <buffer> zzz puts "ZZZ]])
    vim.cmd([[iabbrev <buffer> ppp require 'pp'; pp]])
  end,
})

-- Spelling corrections
vim.cmd([[abbr conosle console]])
vim.cmd([[abbr comopnent component]])

-- Make tildes less visible
vim.api.nvim_set_hl(0, 'EndOfBuffer', { link = 'Comment' })

-- Make menu selections visible
vim.api.nvim_set_hl(0, 'PmenuSel', { ctermfg = 'black', ctermbg = 'magenta' })

--------------------------------------------------------------------------------
-- COLORSCHEME
--------------------------------------------------------------------------------

local function setup_colorscheme()
  pcall(function() vim.cmd('colorscheme molokai') end)
  return true
end

setup_colorscheme()

vim.api.nvim_create_autocmd('VimEnter', {
  callback = function()
    if vim.g.colors_name ~= 'molokai' then
      setup_colorscheme()
    end
  end,
})

--------------------------------------------------------------------------------
-- PLUGIN CONFIGURATIONS
--------------------------------------------------------------------------------

-- nvim-tree
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

-- Telescope
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

-- lualine
pcall(function()
  require('lualine').setup({
    options = {
      theme = 'onedark',
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

-- gitsigns
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

-- treesitter
pcall(function()
  require('nvim-treesitter.configs').setup({
    ensure_installed = {
      'lua', 'vim', 'vimdoc', 'query',
      'javascript', 'typescript', 'html', 'css', 'json',
      'markdown', 'markdown_inline', 'python', 'bash', 'yaml', 'ruby',
      'xml', 'glsl', 'sass', 'scss', 'mermaid',
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

-- nvim-ts-autotag
pcall(function()
  require('nvim-ts-autotag').setup({
    filetypes = {
      'html', 'javascript', 'typescript', 'javascriptreact', 'typescriptreact',
      'svelte', 'vue', 'tsx', 'jsx', 'xml', 'php', 'markdown', 'astro',
      'glimmer', 'handlebars', 'hbs',
    },
    skip_tag_on_closing = false,
  })
end)

-- hlslens
pcall(function()
  require('hlslens').setup({
    calm_down = true,
    nearest_only = true,
    nearest_float_when = 'always',
  })

  local kopts = { noremap = true, silent = true }
  map({ 'n', 'x' }, 'n', [[<Cmd>execute('normal! ' . v:count1 . 'n')<CR><Cmd>lua require('hlslens').start()<CR>]], kopts)
  map({ 'n', 'x' }, 'N', [[<Cmd>execute('normal! ' . v:count1 . 'N')<CR><Cmd>lua require('hlslens').start()<CR>]], kopts)
  map('n', '*', [[*<Cmd>lua require('hlslens').start()<CR>]], kopts)
  map('n', '#', [[#<Cmd>lua require('hlslens').start()<CR>]], kopts)
  map('n', 'g*', [[g*<Cmd>lua require('hlslens').start()<CR>]], kopts)
  map('n', 'g#', [[g#<Cmd>lua require('hlslens').start()<CR>]], kopts)

  vim.api.nvim_create_autocmd('CmdlineEnter', {
    pattern = { '/', '?' },
    callback = function()
      vim.schedule(function() require('hlslens').start() end)
    end,
  })
end)

-- Diagnostics
vim.diagnostic.config({
  virtual_text = {
    severity = { min = vim.diagnostic.severity.WARN },
    source = 'always',
    prefix = '▲',
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

local signs = {
  { name = 'DiagnosticSignError', text = '▲' },
  { name = 'DiagnosticSignWarn', text = '▲' },
  { name = 'DiagnosticSignHint', text = '▲' },
  { name = 'DiagnosticSignInfo', text = '▲' },
}

for _, sign in ipairs(signs) do
  vim.fn.sign_define(sign.name, {
    texthl = sign.name,
    text = sign.text,
    numhl = '',
  })
end

-- Mason
pcall(function()
  require('mason').setup()
  require('mason-lspconfig').setup({
    ensure_installed = { 'lua_ls', 'pyright', 'ts_ls' },
  })
end)

-- LSP
pcall(function()
  local capabilities = require('cmp_nvim_lsp').default_capabilities()

  vim.lsp.config('lua_ls', {
    settings = {
      Lua = {
        runtime = { version = 'LuaJIT' },
        workspace = { checkThirdParty = false },
        telemetry = { enable = false },
        diagnostics = {
          globals = { 'vim' },
        },
      },
    },
    capabilities = capabilities,
  })
  vim.lsp.enable('lua_ls')

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

  vim.lsp.config('ts_ls', {
    capabilities = capabilities,
  })
  vim.lsp.enable('ts_ls')

  -- LSP key mappings
  vim.api.nvim_create_autocmd('LspAttach', {
    group = vim.api.nvim_create_augroup('UserLspConfig', {}),
    callback = function(ev)
      local opts = { buffer = ev.buf }
      map('n', 'gD', vim.lsp.buf.declaration, opts)
      map('n', 'gd', vim.lsp.buf.definition, opts)
      map('n', 'K', vim.lsp.buf.hover, opts)
      map('n', 'gi', vim.lsp.buf.implementation, opts)
      map('n', '<C-k>', vim.lsp.buf.signature_help, opts)
      map('n', '<space>wa', vim.lsp.buf.add_workspace_folder, opts)
      map('n', '<space>wr', vim.lsp.buf.remove_workspace_folder, opts)
      map('n', '<space>wl', function()
        print(vim.inspect(vim.lsp.buf.list_workspace_folders()))
      end, opts)
      map('n', '<space>D', vim.lsp.buf.type_definition, opts)
      map('n', '<space>rn', vim.lsp.buf.rename, opts)
      map({ 'n', 'v' }, '<space>ca', vim.lsp.buf.code_action, opts)
      map('n', 'gr', vim.lsp.buf.references, opts)
    end,
  })
end)

-- which-key
pcall(function()
  require('which-key').setup({
    plugins = {
      marks = true,
      registers = true,
      spelling = { enabled = true },
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

  local cmp_autopairs = require('nvim-autopairs.completion.cmp')
  local cmp = require('cmp')
  cmp.event:on('confirm_done', cmp_autopairs.on_confirm_done())
end)

-- conform.nvim (formatting - this is the ONLY format-on-save)
pcall(function()
  require('conform').setup({
    formatters_by_ft = {
      lua = { 'stylua' },
      python = { 'black', 'isort' },
      javascript = { 'oxc', stop_after_first = true },
      typescript = { 'oxc', stop_after_first = true },
      javascriptreact = { 'oxc', stop_after_first = true },
      typescriptreact = { 'oxc', stop_after_first = true },
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
        args = { 'format', '--stdin-file-path', '$FILENAME' },
      },
      oxc = {
        command = 'oxc',
        args = { 'format', '--stdin-filename', '$FILENAME' },
        stdin = true,
      },
    },
    format_on_save = {
      timeout_ms = 500,
      lsp_fallback = true,
    },
  })

  map({ 'n', 'v' }, '<leader>f', function()
    require('conform').format({ async = true, lsp_fallback = true })
  end, { desc = 'Format buffer' })
end)

-- nvim-cmp (completion)
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

  cmp.setup.cmdline({ '/', '?' }, {
    mapping = cmp.mapping.preset.cmdline(),
    sources = { { name = 'buffer' } },
  })

  cmp.setup.cmdline(':', {
    mapping = cmp.mapping.preset.cmdline(),
    sources = cmp.config.sources({ { name = 'path' } }, { { name = 'cmdline' } }),
  })
end)

--------------------------------------------------------------------------------
-- HOST-SPECIFIC CONFIG
--------------------------------------------------------------------------------

local nvimlocal = vim.fn.expand('~/.config/nvim/nvimlocal.lua')
if vim.fn.filereadable(nvimlocal) == 1 then
  dofile(nvimlocal)
end

-- Fallback to vim version
local nvimlocal_vim = vim.fn.expand('~/.config/nvim/nvimlocal.vim')
if vim.fn.filereadable(nvimlocal_vim) == 1 then
  vim.cmd('source ' .. nvimlocal_vim)
end

-- Clear any startup search highlighting
vim.cmd('silent! nohlsearch')
