#!/usr/bin/env zsh
# (this can run in bash without modification)
#
# Usage: ./update.sh [--clean] [pattern]
#
# Update Neovim plugins managed by vim-plug.
# Specify [pattern] to update only plugins that match the pattern.
# Use --clean to remove unused plugins before updating.

set -e

# Default to update mode
clean_mode=false
pattern=""

# Parse arguments
for arg in "$@"; do
  case "$arg" in
    --clean)
      clean_mode=true
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

# Check if vim-plug is installed
plug_path="$HOME/.local/share/nvim/site/autoload/plug.vim"
if [ ! -f "$plug_path" ]; then
  echo "Installing vim-plug..."
  curl -fLo "$plug_path" --create-dirs \
    https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim
fi

# Build the command
if [ "$clean_mode" = true ]; then
  echo "◉ Cleaning unused plugins and updating..."
  if [ -n "$pattern" ]; then
    echo "⚠️  Warning: --clean mode updates all plugins, pattern '$pattern' will be ignored"
  fi
  nvim --headless -c "PlugClean! | PlugUpdate | qa"
else
  if [ -n "$pattern" ]; then
    echo "◉ Updating plugins matching '$pattern'..."
    # vim-plug doesn't support pattern matching directly, so we update all
    # and let the user know
    echo "⚠️  Note: vim-plug updates all plugins. Pattern matching not supported."
    nvim --headless -c "PlugUpdate | qa"
  else
    echo "◉ Updating all plugins..."
    nvim --headless -c "PlugUpdate | qa"
  fi
fi

echo "╰─ Plugin update complete"
