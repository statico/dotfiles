#--------------------------------------------------------------------------
# zsh 4 keybindings
#--------------------------------------------------------------------------

# first, primarily use emacs key bindings
bindkey -e

# one keystroke to cd ..
bindkey -s '\eu' '^Ucd ..; ls^M'

# AUTO_PUSHD is set so we can always use popd 
bindkey -s '\ep' '^Upopd >/dev/null; dirs -v^M'

# connect to my most recently used screen session
bindkey -s '\ek' "^Urxvt-title ccs;a -t zsh -c 'source .zshrc ; source .zsh/ssh.zsh ; screen -x'^M"

# simple custom bindings
bindkey "^O" copy-prev-shell-word
bindkey "^Q" push-line
bindkey "^T" history-incremental-search-forward
bindkey "ESC-." insert-last-word

# edit the command line with Meta-e
autoload -U edit-command-line
zle -N edit-command-line
bindkey '\ee' edit-command-line

# insert files as command-line arguments
autoload -U insert-files
zle -N insert-files
bindkey '\ei' insert-files

# let ^W delete to slashes - zsh-users list, 4 Nov 2005
backward-delete-to-slash () {
    local WORDCHARS=${WORDCHARS//\//}
    zle .backward-delete-word
}
zle -N backward-delete-to-slash
bindkey "^W" backward-delete-to-slash

# choose from the directory stack - zsh-users list, 29 Oct 2005
select-from-cd-stack() {
    LBUFFER=$LBUFFER"~+"
    zle menu-complete
    if [[ ${LBUFFER[-2,-1]} = "~+" ]]; then
        LBUFFER=${LBUFFER[1,-3]}
    fi
}
zle -N select-from-cd-stack
bindkey "\e[20~" select-from-cd-stack

