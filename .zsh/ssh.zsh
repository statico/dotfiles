#-------------------------------------------------------------------
# SSH hosts, aliases and completion
#-------------------------------------------------------------------

# create login shortcuts from SSH config file, which has 'Host' directives
for host in $(grep -E '^Host +\w+$' $HOME/.ssh/config | awk '{print $2}'); do
    alias $host="ssh $host"
done


# preload SSH completion so that _ssh_hosts can be overridden
autoload _ssh
_ssh

# override _ssh_hosts to use .ssh/config
# -> http://www.zsh.org/mla/users/2003/msg00937.html
_ssh_hosts () {
  if [[ "$IPREFIX" == *@ ]]; then
    _combination -s '[:@]' my-accounts users-hosts "users=${IPREFIX/@}" hosts "$@"
  else
    _combination -s '[:@]' my-accounts users-hosts \
      ${opt_args[-l]:+"users=${opt_args[-l]:q}"} hosts "$@"
      # vims hiliting is stupid, hence this line" 
  fi
  if [[ -r "$HOME/.ssh/config" ]]; then
      local IFS=" 	" key host
      while read key host; do
          if [[ "$key" == (#i)host ]]; then
              _wanted hosts expl host \
                  compadd -M 'm:{a-zA-Z}={A-Za-z} r:|.=* r:|=*' "$@" "$host"
          fi
      done < "$HOME/.ssh/config"
  fi
}

#-------------------------------------------------------------------
# SSH agent tricks
#-------------------------------------------------------------------
# Allow customization to skip this magic
[ -f ~/.ssh/noagenttricks ] && return 0

local agentdir=~/.latestssh
local agentfile=$agentdir/$HOST.sh
mkdir -p $agentdir
chmod 0700 $agentdir >/dev/null

if [ -f $agentfile ]
then
  source $agentfile
fi

if [ ! -e "$SSH_AUTH_SOCK" ] || [ ! -e "/proc/$SSH_AGENT_PID" ]; then
  export SSH_AUTH_SOCK=''
  export SSH_AGENT_PID=''
  [ -n "$SSH_AUTH_SOCK" ] && rm -f $SSH_AUTH_SOCK
  [ -f $agentfile ] && rm $agentfile
fi

latestssh () {
  if [ ! -f $agentfile ]; then
    ssh-agent >$agentfile
    source $agentfile
    ssh-add
  fi
}

latestssh
