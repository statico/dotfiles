# vim:ft=zsh:ts=2:sw=2:sts:et:
#   ___           _      ____   _       ___           __ _
#  |_ _|__ _ _ _ ( )___ |_  /__| |_    / __|___ _ _  / _(_)__ _
#   | |/ _` | ' \|/(_-<  / /(_-< ' \  | (__/ _ \ ' \|  _| / _` |
#  |___\__,_|_||_| /__/ /___/__/_||_|  \___\___/_||_|_| |_\__, |
#                                                         |___/

# INTERNAL UTILITY FUNCTIONS {{{1

# Returns whether the given command is executable or aliased.
_has() {
  return $( whence $1 >/dev/null )
}

# Returns whether the given statement executed cleanly. Try to avoid this
# because this slows down shell loading.
_try() {
  return $( eval $* >/dev/null 2>&1 )
}

# Returns whether the current host type is what we think it is. (HOSTTYPE is
# set later.)
_is() {
  return $( [ "$HOSTTYPE" = "$1" ] )
}

# Returns whether out terminal supports color.
_color() {
  return $( [ -z "$INSIDE_EMACS" ] )
}

# ENVIRONMENT VARIABLES {{{1

# Yes, this defeats the point of the TERM variable, but everything pretty much
# uses modern ANSI escape sequences. I've found that forcing everything to be
# "rxvt" just about works everywhere. (If you want to know if you're in screen,
# use SHLVL or TERMCAP.)
if _color; then
  if [ -n "$ITERM_SESSION_ID" ]; then
    if [ "$TERM" = "screen" ]; then
      export TERM=screen-256color
    else
      export TERM=xterm-256color
    fi
  elif [ "$TERM_PROGRAM" = "Apple_Terminal" ]; then
      export TERM=xterm-256color
  else
    export TERM=rxvt
  fi
else
  export TERM=xterm
fi

# Utility variables.
if which hostname >/dev/null 2>&1; then
  HOSTNAME=`hostname`
elif which uname >/dev/null 2>&1; then
  HOSTNAME=`uname -n`
else
  HOSTNAME=unknown
fi
export HOSTNAME

# HOSTTYPE = { Linux | OpenBSD | SunOS | etc. }
if which uname >/dev/null 2>&1; then
  HOSTTYPE=`uname -s`
else
  HOSTTYPE=unknown
fi
export HOSTTYPE

# PAGER
if [ -n "$INSIDE_EMACS" ]; then
  export PAGER=cat
else
  if _has less; then
    export PAGER=less
    if _color; then
      export LESS='-R'
    fi
  fi
fi

# EDITOR
if _has vim; then
  export EDITOR=vim VISUAL=vim
elif _has vi; then
  export EDITOR=vi VISUAL=vi
elif _has emacs; then
  export EDITOR=emacs VISUAL=emacs
fi

# Overridable locale support.
if [ -z $$LC_ALL ]; then
  export LC_ALL=C
fi
if [ -z $LANG ]; then
  export LANG=en_US
fi

# History control.
SAVEHIST=100000
HISTSIZE=100000
if [ -e ~/priv/ ]; then
  HISTFILE=~/priv/zsh_history
elif [ -e ~/secure/ ]; then
  HISTFILE=~/secure/zsh_history
else
  HISTFILE=~/.zsh_history
fi
if [ -n "$HISTFILE" -a ! -w $HISTFILE ]; then
  echo
  echo "[31;1m HISTFILE [$HISTFILE] not writable! [0m"
  echo
fi

# APPLICATION CUSTOMIZATIONS {{{1

# GNU grep
if _color; then
  export GREP_COLOR='1;32'
fi

# Ack is better than grep
if ! _color; then
  alias ack='ack --nocolor'
fi

# GNU and BSD ls colorization.
if _color; then
  export LS_COLORS='no=00:fi=00:di=01;34:ln=01;36:pi=33:so=01;35:bd=33;01:cd=33;01:or=01;05;37;41:mi=01;37;41:ex=01;32:*.tar=01;31:*.tgz=01;31:*.arj=01;31:*.taz=01;31:*.lzh=01;31:*.zip=01;31:*.z=01;31:*.Z=01;31:*.gz=01;31:*.bz2=01;31:*.bz=01;31:*.tz=01;31:*.rpm=01;31:*.cpio=01;31:*.jpg=01;35:*.gif=01;35:*.bmp=01;35:*.xbm=01;35:*.xpm=01;35:*.png=01;35:*.tif=01;35:'
  export LSCOLORS='ExGxFxdxCxDxDxcxcxxCxc'
  export CLICOLOR=1
fi

# PATH MODIFICATIONS {{{1

# Functions which modify the path given a directory, but only if the directory
# exists and is not already in the path. (Super useful in ~/.zshlocal)

_prepend_to_path() {
  if [ -d $1 -a -z ${path[(r)$1]} ]; then
    path=($1 $path);
  fi
}

_append_to_path() {
  if [ -d $1 -a -z ${path[(r)$1]} ]; then
    path=($path $1);
  fi
}

_force_prepend_to_path() {
  path=($1 ${(@)path:#$1})
}

# Note that there is NO dot directory appended!

_force_prepend_to_path /usr/local/sbin
_force_prepend_to_path /usr/local/bin
_force_prepend_to_path ~/bin
_force_prepend_to_path /usr/local/heroku/bin

_append_to_path /usr/games
_append_to_path /usr/sbin

# Add our docs, too
export INFOPATH=$HOME/.dotfiles/info:$INFOPATH

# ALIASES {{{1

alias Ag='sudo apt-get install'
alias Ai='apt-cache show'
alias Ar='sudo apt-get remove'
alias Arp='sudo apt-get remove --purge'
alias As='apt-cache search'
alias VU='~/.vim/update.sh'
alias ZR='echo "Restarting zsh..." ; exec zsh -l'
alias ZL='vi ~/.zshlocal ~/.zshrc ; ZR'
alias ZU='~/.dotfiles/install.sh ; ZR'
alias ZshInstall='~/.dotfiles/install.sh ; ZR'
alias ZshRehash='. ~/.zshrc'
alias agg='_agg () { ag --group $@ | less }; _agg'
alias aag='agg'
alias bc='bc -l'
alias cr2lf="perl -pi -e 's/\x0d/\x0a/gs'"
alias co='git checkout'
alias curltime='curl -w "@$HOME/.curl-format" -o /dev/null -s'
alias d='docker'
alias df='df -H'
alias dls='dpkg -L'
alias dotenv="eval \$(egrep -v '^#' .env | xargs)"
alias dsl='dpkg -l | grep -i'
alias e='emacs'
alias ec='emacsclient --no-wait'
alias f='fg'
alias f1="awk '{print \$1}'"
alias f2="awk '{print \$2}'"
alias f2k9='f2k -9'
alias f2k='f2 | xargs -t kill'
alias fixssh='eval $(tmux showenv -s SSH_AUTH_SOCK)'
alias g='git'
alias gA='git add --all :/'
alias ga='git add'
alias gaa='gA ; st'
alias gac='git add `git status -uall | egrep "#\tboth modified:" | cut -d: -f2`'
alias gap='clear; git add --all --patch'
alias gca='git commit --amend'
alias gcanv='git commit --amend --no-verify -m'
alias gcnv='git commit --no-verify -m'
alias gcane='git commit --amend --no-edit'
alias gcanes='gcane --gpg-sign'
alias gcanoe='gcne'
alias gcanoes='gcne --gpg-sign'
alias gb='git branch'
alias gbl='git branch -v -a'
alias gd='git diff'
alias gdc='git diff --cached'
alias gdd='git difftool'
alias gdw='git diff -w'
alias gf='git fetch'
alias gfmom='git fetch origin && git merge origin/master'
alias gfrb='git fetch origin && git rebase origin/master'
alias gfrbi='gfrb --interactive'
alias gfa='git fetch --all'
alias gg='git grep'
alias gh='git stash'
alias ghl='git stash list'
alias ghp='git stash pop'
alias ghs='git stash save'
alias ghsp='git stash save --patch'
alias ghw='git stash show -p'
alias gk='gitk >/dev/null 2>&1'
alias gl='git quicklog -n 20'
alias gll='git quicklog-long'
alias gl1='git log -n 1'
alias gm='git merge'
alias gmt='git mergetool'
alias gmom='git merge origin/master'
alias gp='git push'
alias gpgdecrypt='gpg --decrypt-files'
alias gpgencrypt='gpg --default-recipient-self --armor --encrypt-files'
alias gph='git push heroku'
alias gpo='git push origin'
alias gpot='git push origin --tags'
alias gpof='git push origin --force-with-lease'
alias gs='git show -p'
alias gsm='git submodule'
alias gsmu='git submodule update --init --recursive'
alias gu='git unstage'
alias gup='git up'
alias gus='git unstage'
alias gvc='vim `git diff --name-only --diff-filter=U`'
alias gvm='vim `git diff --name-only --diff-filter=M`'
alias i4='sed "s/^/    /"'
alias icat='lsbom -f -l -s -pf'
alias iinstall='sudo installer -target / -pkg'
alias ils='ls /var/db/receipts/'
alias ishow='pkgutil --files'
alias k='tree -h'
alias l="ls -lh"
alias ll="l -a"
alias lt='ls -lt'
alias ltr='ls -ltr'
alias nerdcrap='cat /dev/urandom | xxd | grep --color=never --line-buffered "be ef"'
alias netwhat='lsof -i +c 40'
alias nmu='nodemon =nodeunit'
alias notifydone='terminal-notifier -message Done.'
alias ndu='node --debug-brk =nodeunit'
alias pt='pstree -pul'
alias px='pilot-xfer -i'
alias r='screen -D -R'
alias rake='noglob rake'
alias ri='ri -f ansi'
alias rls='screen -ls'
alias rsync-usual='rsync -azv -e ssh --delete --progress'
alias rxvt-invert="echo -n '[?5t'"
alias rxvt-scrollbar="echo -n '[?30t'"
alias scp='scp -C -p'
alias screen='screen -U'
alias slurp='wget -t 5 -c -nH -r -k -p -N --no-parent'
alias sshx='ssh -C -c blowfish -X'
alias st='git status'
alias stt='git status -uall'
alias t='tmux attach'
alias tree="tree -F -A -I CVS"
alias tt='tail -n 9999'
alias wgetdir='wget -r -l1 -P035 -nd --no-parent'
alias whois='whois -h geektools.com'
alias x='screen -A -x'
alias xxx='histring "XXX.*" -c green -s bold'

# Interactive/verbose commands.
alias mv='mv -i'
for c in cp rm chmod chown rename; do
  alias $c="$c -v"
done

# Make sure vim/vi always gets us an editor.
if _has vim; then
  alias vi=vim
  vs() { vim +"NERDTree $1" }
  gvs() { gvim +"NERDTree $1" }
else
  alias vim=vi
fi
if ! _has gvim && _is Darwin; then
  alias gvim='open -a "MacVim"'
fi

# The Silver Searcher is even faster than Ack.
# https://github.com/ggreer/the_silver_searcher
if _has ag; then
  alias ack=ag
  alias ag='ag --color-path 1\;31 --color-match 1\;32 --color'
fi

# Nico is amazing for showing me this.
alias v='vim -R -'
for i in /usr/share/vim/vim*/macros/less.sh(N) ; do
  alias v="$i"
done

# Linux should definitely have Gnu coreutils, right?
if _is Linux; then
  if _color && _try ls --color; then
    alias ls='ls --color'
  fi
fi

if _is Darwin; then
  alias strace='sudo dtruss -f sudo -u $USER'
fi

# FUNCTIONS {{{1

# ack is really useful. I usually look for code and then edit all of the files
# containing that code. Changing `ack' to `vack' does this for me.
if _has ag; then
  vack() {
    vim `ag --nocolor -l $@`
  }
else
  vack() {
    vim `ack -l $@`
  }
fi
alias vag=vack

# ..same thing with gg.
vgg() {
  vim `gg -l $@`
}

# Quick commands to sync CWD between terminals.
pin() {
  rm -f ~/.pindir
  echo $PWD >~/.pindir
  chmod 0600 ~/.pindir >/dev/null 2>&1
}
pout() {
  cd `cat ~/.pindir`
}

# A quick grep-for-processes.
psl() {
  if _is SunOS; then
    ps -Af | grep -i $1 | grep -v grep
  else
    ps auxww | grep -i $1 | grep -v grep
  fi
}

# Make a new command.
vix() {
  if [ -z "$1" ]; then
    echo "usage: $0 <newfilename>"
    return 1
  fi
  touch $1
  chmod -v 0755 $1
  $EDITOR $1
}

# Make a new command in ~/bin
makecommand() {
  if [ -z "$1" ]; then
    echo "Gotta specify a command name, champ" >&2
    return 1
  fi

  mkdir -p ~/bin
  local cmd=~/bin/$1
  if [ -e $cmd ]; then
    echo "Command $1 already exists" >&2
  else
    echo "#!${2:-/bin/sh}" >$cmd
  fi

  vix $cmd
}

# View a Python module in Vim.
vipy() {
  p=`python -c "import $1; print $1.__file__.replace('.pyc','.py')"`
  if [ $? = 0 ]; then
    vi -R "$p"
  fi
  # errors will be printed by python
}

rxvt-title() {
  echo -n "]2;$*"
}

screen-title() {
  echo -n "k$*\\"
}

# Everything Git-related

# Commit what's been staged, use args as message.
gc() {
  git commit -m "$*"
  git log --oneline --decorate -n 10
}

# Commit everything, use args as message.
sci() {
  if [ $# = 0 ]; then
    echo "usage: $0 message..." >&2
    return 1
  fi
  git add -A
  hr staging
  git status
  hr committing
  git cim "$*"
  hr results
  git quicklog
  hr done
}

# Don't page inside of emacs
if [ -n "$INSIDE_EMACS" ]; then
  alias git='git --no-pager'
fi

# ZSH-SPECIFIC COMPLETION {{{1

# ---------------------------------------------
# The following lines were added by compinstall

zstyle ':completion:*' use-perl true
zstyle ':completion:*' completer _complete _prefix
zstyle ':completion:*' completions 1
zstyle ':completion:*' glob 1
zstyle ':completion:*' group-name ''
zstyle ':completion:*' insert-unambiguous false
zstyle ':completion:*' list-colors "di=01;34:ma=43;30"
zstyle ':completion:*' max-errors 0
zstyle ':completion:*' menu select=0
zstyle ':completion:*' select-prompt %SScrolling active: current selection at %p%s

# Experimental man page parsing.
zstyle ':completion:*:manuals'    separate-sections true
zstyle ':completion:*:manuals.*'  insert-sections   true
zstyle ':completion:*:man:*'      menu yes select

zstyle :compinstall filename "$HOME/.zsh/comp.zsh"

autoload -U compinit
compinit -u
# End of lines added by compinstall
# ---------------------------------------------

# Ignore useless files, like .pyc.
zstyle ':completion:*:(all-|)files' ignored-patterns '(|*/).pyc'

# Completing process IDs with menu selection.
zstyle ':completion:*:*:kill:*' menu yes select
zstyle ':completion:*:kill:*'   force-list always

# Load menu-style completion.
zmodload -i zsh/complist
bindkey -M menuselect '^M' accept

# Specific command completions or overrides.
compdef _perl_modules vipm gvipm
compdef '_command_names -e' tsocks
compdef '_files -g "*.{pdf,ps}"' evince
compdef '_files -g "*.tex"' gen
compdef '_files -g "*.bom"' lsbom

# Show dots while waiting to complete. Useful for systems with slow net access,
# like those places where they use giant, slow NFS solutions. (Hint.)
expand-or-complete-with-dots() {
echo -n "\e[31m......\e[0m"
zle expand-or-complete
zle redisplay
}
zle -N expand-or-complete-with-dots
bindkey "^I" expand-or-complete-with-dots

# This inserts a tab after completing a redirect. You want this.
# (Source: http://www.zsh.org/mla/users/2006/msg00690.html)
self-insert-redir() {
integer l=$#LBUFFER
zle self-insert
(( $l >= $#LBUFFER )) && LBUFFER[-1]=" $LBUFFER[-1]"
}
zle -N self-insert-redir
for op in \| \< \> \& ; do
  bindkey "$op" self-insert-redir
done

# this one's from Ari
# Function Usage: doc packagename
#                 doc pac<TAB>
doc() { cd /usr/share/doc/$1 && ls }
compdef '_files -W /usr/share/doc -/' doc

# Paste the output of the last command.
last-command-output() {
  eval $(fc -l -1 | cut -d\  -f3- | paste -s )
}
zle -N last-command-output
bindkey "^[n" last-command-output

# Automatically quote URLs when pasted
autoload -U url-quote-magic
zle -N self-insert url-quote-magic

# Turn off completion and weirdness if we're within Emacs.
if [[ "$EMACS" = "t" ]]; then
  unsetopt zle
fi

# Turn off slow git branch completion. http://stackoverflow.com/q/12175277/102704
zstyle :completion::complete:git-checkout:argument-rest:headrefs command "git for-each-ref --format='%(refname)' refs/heads 2>/dev/null"

# ZSH KEYBINDINGS {{{1

# First, primarily use emacs key bindings
bindkey -e

# One keystroke to cd ..
bindkey -s '\eu' '\eq^Ucd ..; ls^M'

# Connect to my most recently used screen session
bindkey -s '\ej' "^Uif [ -n \"\$TMUX\" ] ; then ; ssh lemon ; else ; ssh lemon -t tmux att ; fi^M"

# Smart less-adder
bindkey -s "\el" "^E 2>&1|less^M"

# This lets me use ^Z to toggle between open text editors.
bindkey -s '^Z' '^Ufg^M'

# Trying out Facebook PathPicker
bindkey -s '\ex' ' |fpp^M'

# More custom bindings
bindkey "^O" copy-prev-shell-word
bindkey "^Q" push-line
bindkey "^T" history-incremental-search-forward
bindkey "ESC-." insert-last-word

# Edit the current command line with Meta-e
autoload -U edit-command-line
zle -N edit-command-line
bindkey '\ee' edit-command-line

# Let ^W delete to slashes - zsh-users list, 4 Nov 2005
backward-delete-to-slash() {
  local WORDCHARS=${WORDCHARS//\//}
  zle .backward-delete-word
}
zle -N backward-delete-to-slash
bindkey "^W" backward-delete-to-slash

# AUTO_PUSHD is set so we can always use popd
bindkey -s '\ep' '^Upopd >/dev/null; dirs -v^M'

# ZSH OPTIONS {{{1

# Changing Directories
setopt auto_cd
setopt auto_pushd
setopt pushd_ignore_dups
setopt pushd_silent

# Completion
setopt auto_param_slash
setopt complete_in_word
setopt glob_complete
setopt list_beep
setopt list_packed
setopt list_rows_first
setopt no_beep

# History
setopt append_history
unsetopt share_history
unsetopt bang_hist
unsetopt extended_history

# Job Control
setopt notify

# PROMPT AWESOMENESS {{{1

# Unfortunately, ^L makes the first line disappear. We can fix that by making
# our own clear-screen function.
clear-screen-and-precmd() {
  print -n "\e[2J\e[H"
  zle redisplay
  precmd
}
zle -N clear-screen-and-precmd

# This is the easiest way to get a newline. SRSLY.
local __newline="
"

# Unicode looks cool.
if `echo $LANG | grep -E -i 'utf-?8' &>/dev/null`; then
  __sigil="〉"
else
  __sigil="%# "
fi

# Don't use newlines in the prompt because it causes excess scrolling when
# resizing a terminal. The solution is to have precmd() print the first line
# and set PS1 to the second line. See: http://xrl.us/bf3wh

colorprompt() {
  if ! _color; then
    uncolorprompt
    return
  fi

  __prompt_mode=${1:-0}
  local -a line1
  line1=(
    "%{[${__prompt_mode}m%}%~"
    "%(1j.%{[36;1m%} ● %j jobs%{[0m%}.)"
    "%(?..%{[31;1m%} ▲ error %?%{[0m%})"
  )
  local -a line2
  line2=(
    "%{%(!.[31;5m.[${__prompt_mode}m)%}$__sigil%{[0m%}"
  )

  # it's like temp=join("", $promptstring)
  __first_prompt_line=${(j::)line1}

  bindkey "^L" clear-screen-and-precmd
  precmd() { print -P $__first_prompt_line }
  PS1=${(j::)line2}
}

uncolorprompt() {
  local -a temp
  temp=(
    "%m: %~"
    "%(1j. (%j jobs).)"
    "%(?.. (error %?%))"
    $__newline
    "%n $__sigil "
  )
  bindkey "^L" clear-screen
  unfunction precmd &>/dev/null
  PS1=${(j::)temp}
}

shortprompt() {
  __prompt_mode=${__prompt_mode:-0}
  bindkey "^L" clear-screen
  unfunction precmd &>/dev/null
  PS1="%{[${__prompt_mode}m%}$%{[0m%} "
}

simpleprompt() {
  __prompt_mode=${__prompt_mode:-0}
  bindkey "^L" clear-screen
  unfunction precmd &>/dev/null
  PS1="%# "
}

if [ -n "$INSIDE_EMACS" ]; then
  unfunction colorprompt
  unfunction uncolorprompt
  colorprompt() { simpleprompt }
  uncolorprompt() { simpleprompt }
  simpleprompt
elif [ -n "$SUDO_USER" ]; then
  colorprompt '33;1'
else
  colorprompt
fi

# SSH {{{1

# Create login shortcuts from SSH config file, which has 'Host' directives.
# (If you set up an ssh host in .ssh/config, it become an alias, unless an alias
# with that name already exists.)
if [ -e "$HOME/.ssh/config" -a ! -e "$HOME/.ssh/skip-host-aliases" ]; then
  for host in $(grep -E '^Host +\w+$' $HOME/.ssh/config | awk '{print $2}'); do
    if ! _try which $host; then
      alias $host="ssh $host"
    fi
  done
fi

# Override _ssh_hosts to use .ssh/config. This speeds up ssh/scp tab-completion
# *considerably* on instalatios with lots of hosts.
#
# See: http://www.zsh.org/mla/users/2003/msg00937.html
autoload _ssh ; _ssh
_ssh_hosts() {
  if [[ -r "$HOME/.ssh/config" ]]; then
    local IFS="   " key host
    while read key host; do
      if [[ "$key" == (#i)host ]]; then
        _wanted hosts expl host \
          compadd -M 'm:{a-zA-Z}={A-Za-z} r:|.=* r:|=*' "$@" "$host"
      fi
    done < "$HOME/.ssh/config"
  fi
}

# Set up ssh agent if I've been using `keychain`.
for cmd in ~/bin/keychain /usr/bin/keychain; do
  if [ -x "$cmd" ]; then
    keychainbin=$cmd
    break
  fi
done
if [ -n $keychainbin ]; then
  if [ -e  ~/.keychain/${HOSTNAME}-sh ]; then
    source ~/.keychain/${HOSTNAME}-sh >/dev/null 2>&1
  fi
  alias agent="$keychainbin id_dsa && source ~/.keychain/$HOST-sh"
else
  alias agent="echo command not found: keychain"
fi

# A problem with screen is that old sessions lose ssh-agent awareness. This
# little system fixes it.
_fix_old_ssh_agents() {
  local agentdir=~/.latestssh
  local agentfile=$agentdir/$HOST.sh

  mkdir -p $agentdir
  chmod 0700 $agentdir >/dev/null

  if [ -n "$SSH_AUTH_SOCK" -a -z $STY ]; then
    rm -f $agentfile >/dev/null
    echo "export SSH_AUTH_SOCK=$SSH_AUTH_SOCK" >$agentfile
    chmod 0600 $agentfile >/dev/null
  fi

  # ...existing windows can run this alias
  alias latestssh="source $agentfile; ls \$SSH_AUTH_SOCK"

  # ...new windows get it automatically
  if [ -n "$STY" ]; then
    source $agentfile
  fi
}
_fix_old_ssh_agents

# Set terminal colors based on SSH host.
#
# Create a .ssh/colors file with lines like "<hostname> cc33ff colour38" and the color will be set
# automatically. The first color (hex) is used for iTerm and the second (xterm) for tmux.
if [ -n "$TMUX_PANE" ]; then
  termcolor() {
    tmux select-pane -t "$TMUX_PANE" -P "bg=${1}"
  }
elif [ -n "$ITERM_SESSION_ID" ]; then
  termcolor() {
    echo -ne "\033]Ph${1}\033\\"
  }
fi

if whence -w termcolor > /dev/null 2>&1 && [ -e ~/.ssh/colors ]; then
  unfunction ssh >/dev/null 2>&1
  alias realssh="$(which ssh)"
  ssh() {
    local line="$(grep -E "^$1\b" ~/.ssh/colors)"
    local -a args
    if [ -n "$line" ]; then
      args=($(cut -d\  -f2- <<<$line))
      if [ -n "$TMUX_PANE" ]; then
        eval termcolor "${args[2]}"
      else
        eval termcolor "${args[1]}"
      fi
      eval realssh $@
      if [ -n "$TMUX_PANE" ]; then
        eval termcolor default
      else
        eval termcolor 000000
      fi
    else
      eval realssh $@
    fi
  }
fi

# FZF {{{1

# fzf via Homebrew
if [ -e /usr/local/opt/fzf/shell/completion.zsh ]; then
  source /usr/local/opt/fzf/shell/key-bindings.zsh
  source /usr/local/opt/fzf/shell/completion.zsh
fi

# fzf via local installation
if [ -e ~/.fzf ]; then
  _append_to_path ~/.fzf/bin
  source ~/.fzf/shell/key-bindings.zsh
  source ~/.fzf/shell/completion.zsh
fi

# fzf + ag configuration
if _has fzf && _has ag; then
  export FZF_DEFAULT_COMMAND='ag --nocolor -g ""'
  export FZF_CTRL_T_COMMAND="$FZF_DEFAULT_COMMAND"
  export FZF_ALT_C_COMMAND="$FZF_DEFAULT_COMMAND"
  export FZF_DEFAULT_OPTS='
  --color fg:242,bg:236,hl:65,fg+:15,bg+:239,hl+:108
  --color info:108,prompt:109,spinner:108,pointer:168,marker:168
  '
fi

# SOURCE LOCAL CONFIG {{{1

if [ -e ~/.zshlocal ]; then
  . ~/.zshlocal
fi

# }}} Done.

# Don't end with errors.
true
