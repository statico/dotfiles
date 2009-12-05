# vim:ft=zsh:ts=4:sw=4:et:
#
# ╝╔═║╔═ ╝╔═╝  ══║╔═╝║ ║  ╔═╝╔═║╔═ ╔═╝╝╔═╝
# ║╔═║║ ║ ══║  ╔╝ ══║╔═║  ║  ║ ║║ ║╔═╝║║ ║
# ╝╝ ╝╝ ╝ ══╝  ══╝══╝╝ ╝  ══╝══╝╝ ╝╝  ╝══╝
#

# INTERNAL UTILITY FUNCTIONS {{{1

if [ -e ~/.zshdebug ]; then
    function LOG () { echo "[36;1m# $*[0m" }
else
    function LOG () { }
fi
LOG "Starting Zsh"

# Returns whether the given command is executable or aliased.
function _has() {
    return $( whence $1 >/dev/null )
}

# Returns whether the given statement executed cleanly. Try to avoid this
# because this slows down shell loading.
function _try() {
    return $( eval $* >/dev/null 2>&1 )
}

# Returns whether the current host type is what we think it is. (HOSTTYPE is
# set later.)
function _is() {
    return $( [ "$HOSTTYPE" = "$1" ] )
}

# ENVIRONMENT {{{1

# Yes, this defeats the point of the TERM variable. But, face it, everything
# uses modern ANSI escape sequences, and I've found that forcing everything to
# be "rxvt" just about works everywhere. (If you want to know if you're in
# screen, use SHLVL or TERMCAP.)
export TERM=rxvt

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
if _has less; then
    export PAGER=less
    export LESS='-R'
fi

# EDITOR
if _has vim; then
    export EDITOR=vim VISUAL=vim
elif _has vi; then
    export EDITOR=vi VISUAL=vi
fi

# GNU grep
export GREP_OPTIONS='--color=auto'
export GREP_COLOR='1;32'

# GNU and BSD ls colorization.
export LS_COLORS='no=00:fi=00:di=01;34:ln=01;36:pi=33:so=01;35:bd=33;01:cd=33;01:or=01;05;37;41:mi=01;37;41:ex=01;32:*.tar=01;31:*.tgz=01;31:*.arj=01;31:*.taz=01;31:*.lzh=01;31:*.zip=01;31:*.z=01;31:*.Z=01;31:*.gz=01;31:*.bz2=01;31:*.bz=01;31:*.tz=01;31:*.rpm=01;31:*.cpio=01;31:*.jpg=01;35:*.gif=01;35:*.bmp=01;35:*.xbm=01;35:*.xpm=01;35:*.png=01;35:*.tif=01;35:'
export LSCOLORS='ExGxFxdxCxDxDxcxcxxCxc'
export CLICOLOR=1

# Overridable locale support.
if [ -z $$LC_ALL ]; then
    export LC_ALL=C
fi
if [ -z $LANG ]; then
    export LANG=en_US
fi

# PATH MODIFICATIONS {{{1

# Functions which modify the path given a directory, but only if the directory
# exists and is not already in the path.

function _prepend_to_path() {
    if [ -d $1 -a -z ${path[(r)$1]} ]; then
        path=($1 $path);
    fi
}

function _append_to_path() {
    if [ -d $1 -a -z ${path[(r)$1]} ]; then
        path=($1 $path);
    fi
}

# Note that there is NO dot directory appended!

_prepend_to_path ~/bin
_prepend_to_path /usr/local/bin
_prepend_to_path /usr/local/sbin

_append_to_path /usr/games
_append_to_path /usr/X11R6/bin
_append_to_path /usr/local/mysql/bin
_append_to_path /usr/sbin

# ALIASES {{{1

alias Ag='sudo apt-get install'
alias Ai='apt-cache show'
alias Ar='sudo apt-get remove'
alias Arp='sudo apt-get remove --purge'
alias As='apt-cache search'
alias ZR=ZshRehash
alias ZshInstall='~/.dotfiles/install.sh'
alias ZshRehash='. ~/.zshrc'
alias bc='bc -l'
alias cr2lf="perl -pi -e 's/\x0d/\x0a/gs'"
alias df='df -H'
alias dls='dpkg -L'
alias dsl='dpkg -l | grep -i'
alias f1="awk '{print \$1}'"
alias f2="awk '{print \$2}'"
alias f2k9='f2k -9'
alias f2k='f2 | xargs -t kill'
alias gpgdecrypt='gpg --decrypt-files'
alias gpge='gpg --edit-key'
alias gpgencrypt='gpg --default-recipient-self --armor --encrypt-files'
alias gpgrk='gpg --recv-keys'
alias k='tree'
alias l="ls -lh"
alias ll="l -a"
alias ltr='ls -ltr'
alias man='LANG= man'
alias netwhat='lsof -i +c 40'
alias nls='netstat -l | grep tcp'
alias nlsn='netstat -ln | grep tcp'
alias nologout='setopt ignore_eof'
alias pd='perldoc'
alias pt='pstree -pul'
alias px='pilot-xfer -i'
alias r='screen -D -R'
alias ri='ri -f ansi'
alias rls='screen -ls'
alias rsync-usual='rsync -azv -e ssh --delete --progress'
alias rxvt-invert="echo -n '[?5t'"
alias rxvt-scrollbar="echo -n '[?30t'"
alias scp='scp -C -p'
alias screen='screen -U'
alias sd='svn diff --diff-cmd=diff --extensions="-ydw -W$COLUMNS"'
alias sshx='ssh -C -c blowfish -X'
alias svnl='svn log -v -r HEAD'
alias svnvimdiff='svn diff | vim -R +setf\ diff\ nolist -'
alias tree="tree -F -A -I CVS"
alias tt='tail -n 9999'
alias v='vim -R -'
alias wgetdir='wget -r -l1 -P035 -nd --no-parent'
alias whois='whois -h geektools.com'
alias x='screen -A -x'

# Interactive/verbose commands.
alias mv='mv -i'
for c in cp rm chmod chown rename; do
    alias $c="$c -v"
done

# Make sure vim/vi always gets us an editor.
if _has vim; then
    alias vi=vim
    function vs () { vim +"NERDTree $1" }
    function gvs () { gvim +"NERDTree $1" }
else
    alias vim=vi
fi

# Mac OS X doesn't come with wget.
if ! _has wget; then
    alias wget='curl -O'
fi

# Linux should definitely have Gnu coreutils, right?
if _is Linux; then
    if _try ls --color; then
        alias ls='ls --color'
    fi
fi


# COMPLETION {{{1

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
function doc () { cd /usr/share/doc/$1 && ls }
compdef '_files -W /usr/share/doc -/' doc

# Paste the output of the last command.
function last-command-output () {
    eval $(fc -l -1 | cut -d\  -f3- | paste -s )
}
zle -N last-command-output
bindkey "^[n" last-command-output


# PRANK-PROTECTION {{{1

function {
    _try mesg n
    _try mesg -n
}

# FUNCTIONS {{{1

# Quick commands to sync CWD between terminals.
function pin () {
    echo $PWD >~/.pindir
    chmod 0600 ~/.pindir >/dev/null 2>&1
}
function pout () {
    cd `cat ~/.pindir`
}

# Run a command each time I hit 'q', quit on ^|
function loopify () {
    while true; do
        clear
        $@ 2>&1 | less || break
    done
}

# A quick grep-for-processes.
function psl () {
    if _is SunOS; then
        ps -Af | grep -i $1 | grep -v grep
    else
        ps auxww | grep -i $1 | grep -v grep
    fi
}

# RCS helper functions
function st () {
    if [ -d .svn ]; then
        svn status $@
    elif [ -d CVS ]; then
        cvs -n update
    else
        echo "no versioning information found" >&2
        return 1
    fi
}
function stv () {
    if [ -d .svn ]; then
        svn diff | vim -R -
    elif [ -d CVS ]; then
        cvs diff | vim -R -
    else
        echo "no versioning information found" >&2
        return 1
    fi
}
function sci () {
    if [ $# = 0 ]; then
        echo "usage: $0 message..." >&2
        return 1
    fi
    if [ -d .svn ]; then
        svn ci -m "$*"
    elif [ -d CVS ]; then
        cvs ci -m "$*"
    else
        echo "no versioning information found" >&2
        return 1
    fi
}
function svclean () {
    if [ -d .svn ]; then
        svn status | perl -lne'if(/^\?/){ /.{6} (.+)/;system("rm",$1)}'
    elif [ -d CVS ]; then
        echo "not yet implemented for CVS"
    else
        echo "no versioning information found" >&2
        return 1
    fi
}

# Do you miss `git add -A` ?
function svnaddall () {
    svn status | grep \? | f2 | xargs svn add
    svn status | grep \! | f2 | xargs svn rm
}

# Make a new command.
vix () {
    if [ -z "$1" ]; then
        echo "usage: $0 <newfilename>"
        return 1
    fi

    if [ ! -e $1 ]; then
        touch $1
        chmod 0755 $1
    fi

    $EDITOR $1
}

# Make a new command in ~/bin
makecommand () {
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
makeplcommand () {
    makecommand $1 /usr/bin/perl
}
makepycommand () {
    makecommand $1 /usr/bin/python
}

# Remote perldoc
pdoc () {
    w3m -dump http://search.cpan.org/perldoc\?$* | less
}

# Edit a Perl module. (From simon cozens / dnb)
_module_editor_wrapper () {
    cmd=$1
    shift
    if [ "$1" = "-e" ]; then
        perl $@
    else
        $cmd `perldoc -l $1 | sed -e 's/pod$/pm/'`
    fi
}
vipm ()  { _module_editor_wrapper vim  $@ }
gvipm () { _module_editor_wrapper gvim $@ }

# View a Python module in Vim.
function vipy () {
    p=`python -c "import $1; print $1.__file__.replace('.pyc','.py')"`
    if [ $? = 0 ]; then
        vi -R "$p"
    fi
    # errors will be printed by python
}

# Read pod documents nicely.
pless () {
    pod2man $@ | nroff -man | less
}

# Instal a Perl module via apt.
function dpan () {
    pkgs=($(perl -le'print join " ",map {s/::/-/g;lc "lib$_-perl"}@ARGV' $@))
    echo ">> sudo apt-get install $pkgs"
    sudo apt-get install $pkgs
}

# Make an HTML listing for current directory full of images.
makelisting () {
    /bin/ls -1 $@ | \
    perl -MCGI=':standard','*table' -le'
@i=<>;
print start_html({-style=>"font-family:sans-serif;font-size:small"}),
    "<p>These graphics are not the property of this web site and are not
    subject to the license or conditions expressed elsewhere on the site.</p>",
    start_table({-border=>0,-cellspacing=>0,-cellpadding=>3});
while (@i) {
    print td(
        [
            map { td($_) }
            map { qq(<img src="$_" alt="$_" title="$_" align="middle"/>) }
            splice(@i,0,4)
        ]
    ), Tr;
}
print end_table, end_html;
' >index.html

}

function rxvt-title () {
    echo -n "]2;$*"
}

function screen-title () {
    echo -n "k$*\\"
}

# KEYBINDINGS {{{1

# First, primarily use emacs key bindings
bindkey -e

# One keystroke to cd ..
bindkey -s '\eu' '^Ucd ..; ls^M'

# Connect to my most recently used screen session
bindkey -s '\ej' "^Urxvt-title biscotti;c -D 1080 -t zsh -c 'source .zshrc ; source .zsh/ssh.zsh ; screen -U -D -R'^M"

# Smart less-adder
bindkey -s "\el" " 2>&1|less^M"

# More custom bindings
bindkey "^O" copy-prev-shell-word
bindkey "^Q" push-line
bindkey "^T" history-incremental-search-forward
bindkey "ESC-." insert-last-word

# Edit the current command line in Vim with Meta-e
autoload -U edit-command-line
zle -N edit-command-line
bindkey '\ee' edit-command-line

# Let ^W delete to slashes - zsh-users list, 4 Nov 2005
backward-delete-to-slash () {
    local WORDCHARS=${WORDCHARS//\//}
    zle .backward-delete-word
}
zle -N backward-delete-to-slash
bindkey "^W" backward-delete-to-slash

# AUTO_PUSHD is set so we can always use popd
bindkey -s '\ep' '^Upopd >/dev/null; dirs -v^M'

# Choose from the directory stack - zsh-users list, 29 Oct 2005
select-from-cd-stack() {
    LBUFFER=$LBUFFER"~+"
    zle menu-complete
    if [[ ${LBUFFER[-2,-1]} = "~+" ]]; then
        LBUFFER=${LBUFFER[1,-3]}
    fi
}
zle -N select-from-cd-stack
bindkey "\e[20~" select-from-cd-stack

# OPTIONS {{{1

# Changing Directories
setopt auto_cd
setopt auto_pushd
setopt pushd_ignore_dups
setopt pushd_silent

# Completion
setopt auto_param_slash
setopt glob_complete
setopt list_beep
setopt list_packed
setopt list_rows_first
setopt no_beep

# History
setopt append_history
unsetopt bang_hist
unsetopt extended_history

# Job Control
setopt notify

# Input/Output
unsetopt clobber

# HISTORY CONTROL {{{1

SAVEHIST=100000
HISTSIZE=100000
if [ -e ~/priv/ ]; then
    HISTFILE=~/priv/zsh_history
elif [ -e ~/secure/ ]; then
    HISTFILE=~/secure/zsh_history
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

# PROMPT AWESOMENESS {{{1

# This is the easiest way to get a newline. SRSLY.
local newline="
"

# However, we're not going to use newlines because it causes excess scrolling
# when resizing a terminal. The solution is to have precmd() print the first
# line and set PS1 to the second line. See http://xrl.us/bf3wh for more info.

function colorprompt {
    mode=${1:-0}
    line1=(
        "%{[${mode}m%}[%m:%/]"
        "%(1j.%{[36;1m%} (%j jobs)%{[0m%}.)"
        "%(?..%{[31;1m%} (error %?%)%{[0m%})"
    )
    line2=(
        "%{%(!.[31;5m.[${mode}m)%}%n "
        "%#%{[0m%} "
    )

    # it's like temp=join("", $promptstring)
    temp=${(j::)line1}

    precmd() { print -P $temp }
    PS1=${(j::)line2}
}

function uncolorprompt {
    line1=(
        "[%m:%/]"
        "%(1j. (%j jobs).)"
        "%(?.. (error %?%))"
    )
    line2=(
        "%n %# "
    )
    temp=${(j::)line1}
    precmd() { print -P $temp }
    PS1=${(j::)line2}
}

if [ -n "$SUDO_USER" ]; then
    colorprompt '33;1'
else
    colorprompt
fi

# Unfortunately, ^L makes the first line disappear. We can fix that by making
# our own clear-screen function.
clear-screen-and-precmd () {
    print -n "\e[2J\e[H"
    zle redisplay
    precmd
}
zle -N clear-screen-and-precmd
bindkey "^L" clear-screen-and-precmd

# SSH {{{1

# Create login shortcuts from SSH config file, which has 'Host' directives.
# (If you set up an ssh host in .ssh/config, it become an alias.)
for host in $(grep -E '^Host +\w+$' $HOME/.ssh/config | awk '{print $2}'); do
    alias $host="ssh $host"
done

# Override _ssh_hosts to use .ssh/config. This speeds up ssh/scp tab-completion
# *considerably* on instalatios with lots of hosts.
#
# See: http://www.zsh.org/mla/users/2003/msg00937.html
autoload _ssh ; _ssh
function _ssh_hosts () {
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
function {
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

# LOCAL CONFIG {{{1
if [ -e ~/.zshlocal ]; then
    LOG "Sourcing ~/.zshlocal..."
    . ~/.zshlocal
fi

# }}} Done.

# Experimental Additions

setopt share_history
setopt complete_in_word

alias -g ...='../..'
alias -g ....='../../..'
alias -g .....='../../../..'

# Don't end with errors.
LOG "Done."
true
