#!/bin/bash
#
# Sometimes I'm stuck with bash.
#

alias Ag='sudo apt-get install'
alias Ai='apt-cache show'
alias Ar='sudo apt-get remove'
alias Arp='sudo apt-get remove --purge'
alias As='apt-cache search'
alias ZR=ZshRehash
alias ZL='vi ~/.zshlocal ~/.zshrc ; ZR'
alias ZshInstall='~/.dotfiles/install.sh ; ZR'
alias ZshRehash='. ~/.zshrc'
alias bc='bc -l'
alias cr2lf="perl -pi -e 's/\x0d/\x0a/gs'"
alias df='df -H'
alias dls='dpkg -L'
alias dsl='dpkg -l | grep -i'
alias e='emacs'
alias ec='emacsclient --no-wait'
alias f1="awk '{print \$1}'"
alias f2="awk '{print \$2}'"
alias f2k9='f2k -9'
alias f2k='f2 | xargs -t kill'
alias g='git'
alias ga='git add'
alias gap='git add --all --patch'
alias gA='git add --all'
alias gb='git branch'
alias gd='git diff'
alias gdw='git diff -w'
alias gdc='git diff --cached'
alias gfa='git fetch --all'
alias gp='git push'
alias gpo='git push origin'
alias gs='git status'
alias gu='git add --update'
alias gus='git unstage'
alias gl='git quicklog'
alias gpgdecrypt='gpg --decrypt-files'
alias gpge='gpg --edit-key'
alias gpgencrypt='gpg --default-recipient-self --armor --encrypt-files'
alias gpgrk='gpg --recv-keys'
alias k='tree'
alias l="ls -lh"
alias ll="l -a"
alias ltr='ls -ltr'
alias nerdcrap='cat /dev/urandom | xxd | grep --color=never "be ef"'
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
alias slurp='wget -t 5 -c -nH -r -N --no-parent'
alias sshx='ssh -C -c blowfish -X'
alias st='git status'
alias svnl='svn log -v -r HEAD'
alias svnvimdiff='svn diff | vim -R +setf\ diff\ nolist -'
alias tree="tree -F -A -I CVS"
alias tt='tail -n 9999'
alias v='vim -R -'
alias wgetdir='wget -r -l1 -P035 -nd --no-parent'
alias whois='whois -h geektools.com'
alias x='screen -A -x'

ulimit -c 0
if which rbenv > /dev/null; then eval "$(rbenv init -)"; fi
