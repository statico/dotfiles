# adds a game clock to the right side of the penultimate line
# also adds an extended command #time
# by Eidolos

use Time::HiRes;

our $start_time = time;
our $prev_time = $start_time;

our $time_from_start = 0;
our $time_with_trunc = 0;

our $time = 0;        # time as should be reported to user (respecting $trunc)
our $trunc_time = 10; # maximum length of a single frame (seconds)
our $trunc = 1;       # does the user want to truncate times?

extended_command "#time" => sub { $time };

each_iteration
{
    my $now = time;

    $time_from_start = $now - $start_time;

    my $diff = $now - $prev_time;
    $diff = $trunc_time if $diff > $trunc_time;
    $time_with_trunc += $diff;
    $prev_time = $now;

    $time = $trunc ? $time_with_trunc : $time_from_start;
    $botl{time} = serialize_time($time);
}

