#!/usr/bin/env zsh
# Install tree-sitter parsers synchronously via headless nvim.
#
# tree-sitter-manager.nvim installs parsers asynchronously, which means slow
# builds don't finish before nvim exits and they get re-attempted every
# startup. Running them here with vim.wait() blocks until each .dylib lands
# in ~/.local/share/nvim/site/parser/ — after which the plugin sees them and
# stops trying to reinstall.

set -e

if ! command -v nvim &>/dev/null; then
  echo "Error: nvim not found in PATH" >&2
  exit 1
fi

if ! command -v tree-sitter &>/dev/null; then
  echo "Error: tree-sitter CLI not found in PATH" >&2
  exit 1
fi

echo "◉ Installing tree-sitter parsers..."

nvim --headless -c "lua << EOF
local langs = {
  'lua', 'vim', 'vimdoc', 'query',
  'javascript', 'typescript', 'tsx', 'html', 'css', 'json',
  'markdown', 'markdown_inline', 'python', 'bash', 'yaml', 'ruby',
  'xml', 'glsl', 'scss', 'mermaid',
}

local ok, installer = pcall(require, 'tree-sitter-manager.installer')
if not ok then
  io.stderr:write('tree-sitter-manager.installer not available — run lazy install first\n')
  vim.cmd('cq')
end
local util = require('tree-sitter-manager.util')
local config = require('tree-sitter-manager.config')

vim.fn.mkdir(config.cfg.parser_dir, 'p')
vim.fn.mkdir(config.cfg.query_dir, 'p')

local pending = 0
local failed = {}

for _, lang in ipairs(langs) do
  local already
  if installer.is_only_query(lang) then
    already = vim.uv.fs_stat(util.qpath(lang)) ~= nil
  else
    already = vim.uv.fs_stat(util.ppath(lang)) ~= nil
  end
  if not already then
    pending = pending + 1
    installer.install(lang, function(success)
      if not success then table.insert(failed, lang) end
      pending = pending - 1
    end)
  end
end

if pending == 0 then
  print('all parsers already installed')
else
  print('waiting on ' .. pending .. ' parser install(s)...')
  local done = vim.wait(600000, function() return pending == 0 end, 200)
  if not done then
    io.stderr:write('timeout: ' .. pending .. ' install(s) still pending\n')
    vim.cmd('cq')
  end
end

if #failed > 0 then
  io.stderr:write('failed: ' .. table.concat(failed, ', ') .. '\n')
  vim.cmd('cq')
end
EOF" -c qa

echo "╰─ Parser install complete"
