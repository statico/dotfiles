# adds a fortune to the login screen
# by Eidolos

our $fortune = "$ENV{HOME}/.fortune/nethackidiocy";

each_iteration
{
    if ($at_login)
    {
        $postprint .= "\e[s\e[20H\e[1;30m"
                    . `fortune -n200 -s $fortune`
                    . "\e[0m\e[u"
    }
}

