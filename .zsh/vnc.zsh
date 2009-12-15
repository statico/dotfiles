#--------------------------------------------------------------------------
# vnc -- thanks to Jay!
#--------------------------------------------------------------------------

if true; then
    local vnchost=utopia.ccs.neu.edu
    which tightvncserver >/dev/null 2>&1 && vncserverprefix=tight
    which tightvncviewer >/dev/null 2>&1 && vncviewerprefix=tight
    alias vncstartserver="${vncserverprefix}vncserver :2 -alwaysshared -nolisten local -depth 24 -geometry 1024x768"
    alias vnctunnel="while true; do ssh -L 5902:${vnchost}:5902 ${USER}@${vnchost}; done"
    alias vnc="${vncviewerprefix}vncviewer -passwd ~/.vnc/passwd"
    alias vncsecure='vnc -shared localhost:2 -geometry 450x400 '
    alias vncinsecure="echo 'are you insane?'"
fi
