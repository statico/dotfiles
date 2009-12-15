#--------------------------------------------------------------------------
# functions
#--------------------------------------------------------------------------

# RCS helper functions
function st () {
    if [ -d .svn ]; then
        svn status
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

# make a new command
makecommand () {
    if [ $# != 1 ]; then
        echo "Gotta specify a command name, champ" >&2
        return 1
    fi

    mkdir -p ~/bin
    c=~/bin/$1
    if [ -e $c ]; then
        echo "Command $1 already exists" >&2
        return 1
    fi

    echo "#!/bin/sh" >$c
    chmod 0755 $c
    $EDITOR $c
}
makeplcommand () {
    if [ $# != 1 ]; then
        echo "Gotta specify a command name, champ" >&2
        return 1
    fi

    mkdir -p ~/bin
    c=~/bin/$1
    if [ -e $c ]; then
        echo "Command $1 already exists" >&2
        return 1
    fi

    echo "#!/usr/bin/perl" >$c
    chmod 0755 $c
    $EDITOR $c
}

# remote perldoc
pdoc () {      
    w3m -dump http://search.cpan.org/perldoc\?$* | less
}

# from simon cozens / dnb -- give it the name of a module
module_editor_wrapper () {
    cmd=$1
    shift
    if [ "$1" = "-e" ]; then
        perl $@
    else
        $cmd `perldoc -l $1 | sed -e 's/pod$/pm/'`
    fi
}
vipm ()  { module_editor_wrapper vim  $@ }
gvipm () { module_editor_wrapper gvim $@ }

# slide show of sorts
showpics () {
    for i in $@; do
        echo "showing $i"
        display $i
    done
}

# read pod documents nicely
pless () {
    pod2man $@ | nroff -man | less
}

# setup ruby lib path
rubylib () {
    export RUBYLIB=$PWD/lib
    ls -d $RUBYLIB
}

# setup perl lib path
perl5lib () {
    export PERL5LIB=$PWD/lib
    ls -d $PERL5LIB
}

# quick calculator
p () {
    autoload -U zcalc
    zcalc
}

# crypt
crypt () {
	if [[ -z "$2" ]]; then
		echo "usage: crypt <plaintext> <salt>"
	else
		perl -e "print crypt(\"$1\",\"$2\"), \"\\n\""
	fi
}

# make html listing for current directory full of images
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

# fetch an RFC to stdout
rfc () {
    curl -q ftp://ftp.rfc-editor.org/in-notes/rfc$1.txt
}

# make `more` the $PAGER
more () {
  $PAGER $@
}

ls () {
  /bin/ls --color --hide '*.pyc' $@
}
