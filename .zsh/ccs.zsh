#--------------------------------------------------------------------------
# NU CCIS handyisms
#--------------------------------------------------------------------------
if [ -d /net/ccs ]; then

# general aliases
alias peek='ypcat passwd | grep -i'

# no core dumps. we've got quotas!
limit coredumpsize 0

for dir in /net/ccis/bin; do
    if [ -d $dir -a -z ${path[(r)$dir]} ]; then
        path=($path $dir);
    fi
done

# end dependency
fi
