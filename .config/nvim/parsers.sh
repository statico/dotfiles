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

nvim --headless -c "luafile $(dirname "$0")/parsers.lua" -c qa

echo "╰─ Parser install complete"
