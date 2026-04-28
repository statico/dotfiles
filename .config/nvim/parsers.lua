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
