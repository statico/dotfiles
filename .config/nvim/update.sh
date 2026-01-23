#!/usr/bin/env zsh
# (this can run in bash without modification)
#
# Usage: ./update.sh [--clean] [--verbose] [pattern]
#
# Update Neovim plugins managed by lazy.nvim.
# Specify [pattern] to update only plugins that match the pattern.
# Use --clean to remove unused plugins before updating.
# Use --verbose to show full output (default is quiet).

set -e

# Default to update mode
clean_mode=false
verbose=false
pattern=""

# Parse arguments
for arg in "$@"; do
  case "$arg" in
    --clean)
      clean_mode=true
      ;;
    --verbose|-v)
      verbose=true
      ;;
    *)
      if [ -z "$pattern" ]; then
        pattern="$arg"
      fi
      ;;
  esac
done

# Check if nvim is available
if ! command -v nvim &> /dev/null; then
  echo "Error: nvim is not installed or not in PATH"
  exit 1
fi

# Build the command with quiet output by default
if [ "$clean_mode" = true ]; then
  echo "◉ Cleaning unused plugins and updating..."
  if [ -n "$pattern" ]; then
    echo "⚠️  Warning: --clean mode updates all plugins, pattern '$pattern' will be ignored"
  fi
  if [ "$verbose" = true ]; then
    nvim --headless -c "lua require('lazy').clean()" -c "lua require('lazy').update()" -c "qa"
  else
    nvim --headless -c "lua require('lazy').clean({ show = false })" -c "lua require('lazy').update({ show = false })" -c "qa" >/dev/null 2>&1
  fi
else
  if [ -n "$pattern" ]; then
    echo "◉ Updating plugins matching '$pattern'..."
    echo "⚠️  Note: Lazy.nvim pattern matching works best through :Lazy UI"
    echo "   Updating all plugins (use :Lazy in Neovim for selective updates)"
    if [ "$verbose" = true ]; then
      nvim --headless -c "lua require('lazy').update()" -c "qa"
    else
      nvim --headless -c "lua require('lazy').update({ show = false })" -c "qa" >/dev/null 2>&1
    fi
  else
    echo "◉ Updating all plugins..."
    if [ "$verbose" = true ]; then
      nvim --headless -c "lua require('lazy').update()" -c "qa"
    else
      nvim --headless -c "lua require('lazy').update({ show = false })" -c "qa" >/dev/null 2>&1
    fi
  fi
fi

echo "╰─ Plugin update complete"
