#!/bin/bash
#
# Sometimes I'm stuck with bash.
#

export PATH=$HOME/bin:$PATH

alias Ag='sudo apt-get install'
alias Ai='apt-cache show'
alias Ar='sudo apt-get remove'
alias Arp='sudo apt-get remove --purge'
alias As='apt-cache search'
alias ZR=ZshRehash
alias ZL='vi ~/.zshlocal ; ZR'
alias ZshInstall='~/.dotfiles/install.sh ; ZR'
alias ZshRehash='. ~/.zshrc'
alias bc='bc -l'
alias cr2lf="perl -pi -e 's/\x0d/\x0a/gs'"
alias df='df -H'
alias dls='dpkg -L'
alias dsl='dpkg -l | grep -i'
alias e='emacs'
alias ec='emacsclient --no-wait'
alias f='fg'
alias f1="awk '{print \$1}'"
alias f2="awk '{print \$2}'"
alias f2k9='f2k -9'
alias f2k='f2 | xargs -t kill'
alias g='git'
alias gA='git add --all :/'
alias ga='git add'
alias gac='git add `git status -uall | egrep "#\tboth modified:" | cut -d: -f2`'
alias gap='clear; git add --all --patch'
alias gb='git branch'
alias gd='git diff'
alias gdc='git diff --cached'
alias gdd='git difftool'
alias gdw='git diff -w'
alias gf='git fetch'
alias gfmom='git fetch origin && git merge origin/master'
alias gfrb='git fetch origin && git rebase origin/master'
alias gfa='git fetch --all'
alias gh='git stash'
alias ghl='git stash list'
alias ghp='git stash pop'
alias ghs='git stash save'
alias ghsp='git stash save --patch'
alias ghw='git stash show -p'
alias gk='gitk >/dev/null 2>&1'
alias gl='git quicklog -n 20'
alias gll='git quicklog-long'
alias gm='git merge'
alias gmt='git mergetool'
alias gmom='git merge origin/master'
alias gp='git push'
alias gpgdecrypt='gpg --decrypt-files'
alias gpgencrypt='gpg --default-recipient-self --armor --encrypt-files'
alias gph='git push heroku'
alias gpo='git push origin'
alias gs='git show -p'
alias gsm='git submodule'
alias gsmu='git submodule update --init --recursive'
alias gu='git add --update'
alias gup='git up'
alias gus='git unstage'
alias gvc='vim `git diff --name-only --diff-filter=U`'
alias i4='sed "s/^/    /"'
alias icat='lsbom -f -l -s -pf'
alias iinstall='sudo installer -target / -pkg'
alias ils='ls /var/db/receipts/'
alias ishow='pkgutil --files'
alias k='tree'
alias l="ls -lh"
alias ll="l -a"
alias lt='ls -lt'
alias ltr='ls -ltr'
alias nerdcrap='cat /dev/urandom | xxd | grep --color=never --line-buffered "be ef"'
alias netwhat='lsof -i +c 40'
alias nmu='nodemon =nodeunit'
alias ndu='node --debug-brk =nodeunit'
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
alias slurp='wget -t 5 -c -nH -r -k -p -N --no-parent'
alias sshx='ssh -C -c blowfish -X'
alias st='git status'
alias stt='git status -uall'
alias tree="tree -F -A -I CVS"
alias tt='tail -n 9999'
alias wgetdir='wget -r -l1 -P035 -nd --no-parent'
alias whois='whois -h geektools.com'
alias x='screen -A -x'

ulimit -c 0

if which rbenv > /dev/null; then eval "$(rbenv init -)"; fi

export LS_COLORS='rs=0:di=01;34:ln=01;36:mh=00:pi=40;33:so=01;35:do=01;35:bd=40;33;01:cd=40;33;01:or=40;31;01:su=37;41:sg=30;43:ca=30;41:tw=30;42:ow=34;42:st=37;44:ex=01;32:*.tar=01;31:*.tgz=01;31:*.arj=01;31:*.taz=01;31:*.lzh=01;31:*.lzma=01;31:*.tlz=01;31:*.txz=01;31:*.zip=01;31:*.z=01;31:*.Z=01;31:*.dz=01;31:*.gz=01;31:*.lz=01;31:*.xz=01;31:*.bz2=01;31:*.bz=01;31:*.tbz=01;31:*.tbz2=01;31:*.tz=01;31:*.deb=01;31:*.rpm=01;31:*.jar=01;31:*.war=01;31:*.ear=01;31:*.sar=01;31:*.rar=01;31:*.ace=01;31:*.zoo=01;31:*.cpio=01;31:*.7z=01;31:*.rz=01;31:*.jpg=01;35:*.jpeg=01;35:*.gif=01;35:*.bmp=01;35:*.pbm=01;35:*.pgm=01;35:*.ppm=01;35:*.tga=01;35:*.xbm=01;35:*.xpm=01;35:*.tif=01;35:*.tiff=01;35:*.png=01;35:*.svg=01;35:*.svgz=01;35:*.mng=01;35:*.pcx=01;35:*.mov=01;35:*.mpg=01;35:*.mpeg=01;35:*.m2v=01;35:*.mkv=01;35:*.webm=01;35:*.ogm=01;35:*.mp4=01;35:*.m4v=01;35:*.mp4v=01;35:*.vob=01;35:*.qt=01;35:*.nuv=01;35:*.wmv=01;35:*.asf=01;35:*.rm=01;35:*.rmvb=01;35:*.flc=01;35:*.avi=01;35:*.fli=01;35:*.flv=01;35:*.gl=01;35:*.dl=01;35:*.xcf=01;35:*.xwd=01;35:*.yuv=01;35:*.cgm=01;35:*.emf=01;35:*.axv=01;35:*.anx=01;35:*.ogv=01;35:*.ogx=01;35:*.aac=00;36:*.au=00;36:*.flac=00;36:*.mid=00;36:*.midi=00;36:*.mka=00;36:*.mp3=00;36:*.mpc=00;36:*.ogg=00;36:*.ra=00;36:*.wav=00;36:*.axa=00;36:*.oga=00;36:*.spx=00;36:*.xspf=00;36:'

export PS1='\[\033[01;32m\]\u\[\033[00m\]@\[\033[01;32m\]\h\[\033[00m\]\[\033[01;34m\]\w:\[\033[0;32m\]\[\033[00m\]$\[\033[00m\] '

if [ -e ~/.bashlocal ]; then
    . ~/.bashlocal
fi
