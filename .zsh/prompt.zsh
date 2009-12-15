#-------------------------------------------------------------------
# zsh 4 paths
#-------------------------------------------------------------------

# Sometimes I get the zsh-3 docs. That's not good.
export MANPATH=/priv/unix/packages/zsh-4.0.6/man:/priv/unix/packages/zsh-4.0.4/man:$MANPATH

# compinit stuff
if [ -e ~/.zsh/comp.zsh ]; then
	source ~/.zsh/comp.zsh
fi

# prompt
function colorprompt {
    PROMPT="%{[32;1m%}[%m:%~] %(?..%{[31;1m%}(error %?%))
%{%(!.[31;5m.[32;1m)%}%n %#%{[0m%} "
}
function altcolorprompt {
    PROMPT="%{[33;1m%}[%m:%~] %(?..%{[31;1m%}(error %?%))
%{%(!.[31;5m.[33;1m)%}%n %#%{[0m%} "
}
function semicolorprompt {
    PROMPT="[%m:%~] %(?..(error %?%))
%{%(!.[31;5m.)%}%n %#%{%(!.[0m.)%} "
}
function uncolorprompt {
    PROMPT="[%m:%~] %(?..(error %?%))
%n %# "
}

if [ -n "$SUDO_USER" ]; then
    altcolorprompt
elif [ $SHLVL != 1 ]; then
    uncolorprompt
else
    semicolorprompt
fi

