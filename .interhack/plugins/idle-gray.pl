# recolors >59s idle people gray in the to-watch menu
# by Eidolos

each_iteration
{
    for (1..24)
    {
        if ($vt->row_plaintext($_) =~ /^ .\) (\w+)\s+\(\s*\d+x\s*\d+\) \d\d\d\d-\d\d-\d\d \d\d:\d\d:\d\d \((\d+)m \d+s idle\)\s*$/)
        {
            my $color = '';
            my $nick = $1;
            $nick .= ' ' x (15 - length($nick));
            $color = "\e[1;30m" if $2;
            $postprint .= "\e[s\e[${_};5H$color$1\e[m\e[u";
        }
    }
}

