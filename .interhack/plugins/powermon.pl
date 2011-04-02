# adds color to the power display
# by Eidolos

include "stats";

each_iteration
{
    $botl{pw} =~ s{Pw:(-?\d+\(-?\d+\))}{
        my $color = '';

           if ($curpw     >= $maxpw) {                     }
        elsif ($curpw * 2 >= $maxpw) { $color = "\e[1;36m" }
        elsif ($curpw * 3 >= $maxpw) { $color = "\e[1;35m" }
        else                 { $color = "\e[0;35m" }

        "Pw:$color$1\e[0m"
    }eg;
}

