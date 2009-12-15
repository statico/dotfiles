#--------------------------------------------------------------------------
# zshrc 
#--------------------------------------------------------------------------

# handy host variabels
if [ -z $HOSTNAME ]; then
	if which hostname >/dev/null 2>&1; then
		HOSTNAME=`hostname`
	elif which uname >/dev/null 2>&1; then
		HOSTNAME=`uname -n`
	else
		HOSTNAME=unknown fi
fi
if [ -z $HOSTTYPE ]; then
	if which uname >/dev/null 2>&1; then
		HOSTTYPE=`uname -s`
	else
		HOSTTYPE=unknown
	fi
fi
export HOSTNAME HOSTTYPE 

# source all the configs in .zsh, using precompiled .zwc files if possible
for file in ~/.zsh/[[:alnum:]]*.zsh; . $file

# and, finally, source the local config
[ -e ~/.zsh_local.zsh ] && . ~/.zsh_local.zsh

# don't end with errors
true
