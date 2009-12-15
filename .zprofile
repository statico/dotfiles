#--------------------------------------------------------------------------
# fancy header fun stuff
#--------------------------------------------------------------------------

# cluster jobs don't need my prompt atop output
if [ -z $LOADL_ACTIVE ]; then

    # clear the screen
    print -N "\e[2J\e[H"

    # have i set up a custom banner?
    if [ -x ~/bin/loginbanner ]; then
        ~/bin/loginbanner
    
    # if no custom banner, print uname and uptime
    else
        local hname="$(uname -n 2>/dev/null)"
        local os="$(uname -s 2>/dev/null)"
        local arch=""
        if [ uname -p >/dev/null 2>&1 ]; then
            arch="$(uname -p 2>/dev/null)"
        else
            arch="$(uname -m 2>/dev/null)"
        fi
        print -N "\e[44;1m $hname \e[0m\e[44m ( $arch / $os ) \n"
        uptime 2>/dev/null
        print -N "\e[0m\n"
    fi
fi

