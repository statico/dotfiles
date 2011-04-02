our %botl;

include "stats";
include "hpmon";
include "powermon";
include "eido-colors-status";
include "exp-2nl";
include "timer";

our $statusline ||= sub { "$botl{char}  $botl{stats}  $botl{score}" };
our $botl ||= sub { "$botl{dlvl} $botl{au} $botl{hp} $botl{pw} $botl{ac} $botl{xp} $botl{turncount} $botl{status}" };

for my $item (qw/char stats score dlvl gold hp pw ac xp turncount status/)
{
    extended_command $item => sub { "$botl{$item}" };
}

each_iteration
{
    $postprint .= "\e[s";
    my $sl = $statusline->();
    my $bl = $botl->();
    $postprint .= "\e[23;1H\e[K\e[0m$sl" if $show_sl;
    $postprint .= "\e[24;1H\e[K\e[0m$bl" if $show_bl;
    $postprint .= "\e[u";
}
