# this is broken up so we can programmatically color truncated forms of stats
# consider a really long botl that ends with "Ill Burde" - clearly that's
# Burdened but we can't display it, but we should color it
# the cutoff is three characters so as to minimize false positives

include "stats";

# pre-code {{{
my %status_colors =
(
# }}}

# hunger {{{
"Satiated" => "red",
"Hungry"   => "red",
"Weak"     => "bred",
"Fainting" => "bred",
"Fainted"  => "bred",
# }}}

# minor status effects {{{
"Stun"  => "yellow",
"Conf"  => "yellow",
"Hallu" => "yellow",
"Blind" => "yellow",
# }}}

# major status effects {{{
"Ill"      => "bred",
"FoodPois" => "bred",
# }}}

# burden level {{{
"Burdened"   => "yellow",
"Stressed"   => "red",
"Strained"   => "bred",
"Overtaxed"  => "bred",
"Overloaded" => "bred",
# }}}

# post-code {{{
);

each_iteration
{
    my @status_effects;

    STATUS: for my $status (split /\s+/, $botl{status}) {
        for (reverse 3..10) # Overloaded has length 10
        {
            while (my ($k, $v) = each(%status_colors))
            {
                next if $_ > length $k;
                my $status_trunc = substr($k, 0, $_);
                if ($status =~ s/$status_trunc/$colormap{$v}$&\e[0m/) {
                    push @status_effects, $status;
                    keys %status_colors; # reset the internal iterator
                    next STATUS;
                }
            }
        }
    }

    $botl{status} = join " ", @status_effects;
}

# }}}

