#--------------------------------------------------------------------------
# environment
#--------------------------------------------------------------------------

# no messages
(
    mesg n
    mesg -n
) >/dev/null 2>&1

# path (note that there is NO dot directory appended!)
# ...to add to front:
for dir in ~/bin ~/www/bin /usr/local/bin /usr/local/sbin; do
    if [ -d $dir -a -z ${path[(r)$dir]} ]; then
        path=($dir $path);
    fi
done
# ...to add to back:
for dir in /usr/X11R6/bin /usr/local/mysql/bin /usr/sbin /usr/bin /sbin /bin; do
    if [ -d $dir -a -z ${path[(r)$dir]} ]; then
        path=($path $dir);
    fi
done

# terminal is usually xterm-compatible
#export TERM=rxvt

# try setting our pager to less, then more, then nothing
if which less >/dev/null 2>&1; then
	PAGER=less
	export LESS="-arCif"
elif which more >/dev/null 2>&1; then
	PAGER=more
fi
export PAGER

# use vim
if which vim >/dev/null 2>&1; then
	export EDITOR=vim VISUAL=vim
else
	export EDITOR=vi VISUAL=vi
fi

# GNU grep
if grep --color=auto 0 /etc/passwd >/dev/null 2>&1; then
    export GREP_OPTIONS='--color=auto'
    export GREP_COLOR='1;32'
fi

# GNU ls
export LS_COLORS='no=00:fi=00:di=01;34:ln=01;36:pi=33:so=01;35:bd=33;01:cd=33;01:or=01;05;37;41:mi=01;05;37;41:ex=01;32:*.tar=01;31:*.tgz=01;31:*.arj=01;31:*.taz=01;31:*.lzh=01;31:*.zip=01;31:*.z=01;31:*.Z=01;31:*.gz=01;31:*.bz2=01;31:*.bz=01;31:*.tz=01;31:*.rpm=01;31:*.cpio=01;31:*.jpg=01;35:*.gif=01;35:*.bmp=01;35:*.xbm=01;35:*.xpm=01;35:*.png=01;35:*.tif=01;35:'

# locale support
if [ -z $$LC_ALL ]; then
    export LC_ALL=C
fi
if [ -z $LANG ]; then
    export LANG=en_US
fi

# fink
if [ -e /sw/bin/init.sh ]; then
	source /sw/bin/init.sh
fi

# postgresql
for i in /usr/local/pgsql/data /var/lib/postgresql/data; do
	[ -d $i ] && export PGDATA=$i
done

