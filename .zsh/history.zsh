#--------------------------------------------------------------------------
# history control
#--------------------------------------------------------------------------

# zsh history
SAVEHIST=20000
HISTSIZE=20000
if [ -e ~/priv/ ]; then
	HISTFILE=~/priv/zsh_history
else
	HISTFILE=~/.zsh_history
fi

# make sure history file isn't owned by root
# (a common problem)
if [ -n "$HISTFILE" -a ! -w $HISTFILE ]; then
    echo
    echo "[31;1;5m HISTFILE [$HISTFILE] not writable! [0m"
    echo
fi

