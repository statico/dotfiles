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

local failed = {}

-- Install serially: parallel installs cause shared dependencies (e.g. ecma,
-- typescript) to be re-cloned and re-built once per dependent parser, since
-- installer.install does not dedupe in-flight work. Sequential lets each
-- finished parser short-circuit the next one's dep checks.
for _, lang in ipairs(langs) do
  local already
  if installer.is_only_query(lang) then
    already = vim.uv.fs_stat(util.qpath(lang)) ~= nil
  else
    already = vim.uv.fs_stat(util.ppath(lang)) ~= nil
  end
  if not already then
    local done = false
    local result_success = false
    installer.install(lang, function(success)
      result_success = success
      done = true
    end)
    local ok_wait = vim.wait(300000, function() return done end, 100)
    if not ok_wait or not result_success then
      table.insert(failed, lang)
    end
  end
end

if #failed > 0 then
  io.stderr:write('failed: ' .. table.concat(failed, ', ') .. '\n')
  vim.cmd('cq')
end
