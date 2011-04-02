# better color the HP display
# prayer-safe HP is colored as dark gray
# it also doesn't "miss" like the real hpmon
# by Eidolos

include "stats";

our $notified = 0;

each_iteration
{
  $botl{hp} =~ s{(HP:)(\d+\(\d+\))}{
    my $color = '';

       if ($curhp * 7 <= $maxhp || $curhp <= 5) { $color = "\e[1;30m" }
    elsif ($curhp     >= $maxhp)            {                     }
    elsif ($curhp * 2 >= $maxhp)            { $color = "\e[1;32m" }
    elsif ($curhp * 3 >= $maxhp)            { $color = "\e[1;33m" }
    elsif ($curhp * 4 >= $maxhp)            { $color = "\e[0;31m" }
    else                                    { $color = "\e[1;31m" }

    if ($curhp > 0 && ($curhp * 7 <= $maxhp || $curhp <= 5))
    {
        if (!$notified)
        {
            push @postonce, sub { force_tab("Your HP is monstrously low!") };
            $notified = 1;
        }
    }
    else
    {
        $notified = 0;
    }

    "$1$color$2\e[0m"
  }eg;
}

