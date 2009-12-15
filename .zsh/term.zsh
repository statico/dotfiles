#-------------------------------------------------------------------
# rxvt/rxvt-unicode settings
#-------------------------------------------------------------------
alias rxvt-invert="echo -n '[?5t'"
alias rxvt-scrollbar="echo -n '[?30t'"

# not specific to rxvt, really
function rxvt-title () {
    echo -n "]2;$*"
}

function screen-title () {
    echo -n "k$*\\"
}

#-------------------------------------------------------------------
# set the title of the window / screen to the host if we've ssh'd in
#-------------------------------------------------------------------
if [ -n "$SSH_TTY" ]; then
    rxvt-title "$USER@$HOSTNAME"
fi
if [ -n "$STY" ]; then
    screen-title $HOSTNAME
fi

#-------------------------------------------------------------------
# set the title of the xterm to the current command
#-------------------------------------------------------------------
if [ -n "$(tty)" ]; then
    rxvt-title "$USER@$HOSTNAME"
    preexec () { rxvt-title "$USER@$HOSTNAME - ${${(z)*}[1]}" }
fi

