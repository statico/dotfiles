#--------------------------------------------------------------------------
# everything below here is automatically added by compinstall
#--------------------------------------------------------------------------

# The following lines were added by compinstall

zstyle ':completion:*' completer _complete _correct _approximate _prefix
zstyle ':completion:*' completions 1
zstyle ':completion:*' glob 1
zstyle ':completion:*' group-name ''
zstyle ':completion:*' insert-unambiguous false
zstyle ':completion:*' list-colors ${(s.:.)LS_COLORS}
zstyle ':completion:*' max-errors 3
zstyle ':completion:*' substitute 0
zstyle :compinstall filename "$HOME/.zsh/comp.zsh"

autoload -U compinit
compinit -u
# End of lines added by compinstall

#--------------------------------------------------------------------------
# custom completers
#--------------------------------------------------------------------------

# this one's from Ari
# Function Usage: doc packagename
#                 doc pac<TAB>
function doc () { cd /usr/share/doc/$1 && ls }
compdef '_files -W /usr/share/doc -/' doc

# other completions
compdef _perl_modules vipm gvipm
compdef '_command_names -e' tsocks
compdef '_files -g "*.{pdf,ps}"' evince
compdef '_files -g "*.tex"' gen
