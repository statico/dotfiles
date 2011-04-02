# improves exp display to show how much more for next level
# also adds an extended command #exp for a summary
# by Eidolos with help from arcanehl

include "stats";

my @exp_for = qw/0 0 20 40 80 160 320 640 1280 2560 5120 10000 20000 40000
80000 160000 320000 640000 1280000 2560000 5120000 10000000 20000000 30000000
40000000 50000000 60000000 70000000 80000000 90000000 100000000/;

sub exp_for
{
  my $level = shift;
  return 0 if $level < 1 || $level > 30;
  return $exp_for[$level];
}

our $exp_needed;

extended_command "#exp"
              => sub
                 {
                   return "Level: 30. Experience: $xp."
                     if $xlvl == 30;

                   sprintf 'Level: %d. Experience: %d. '
                         . 'Next in: %d. Progress: %.2f%%.',
                             $xlvl,
                             $xp,
                             $exp_needed,
                             100 * ($xp        - exp_for($xlvl))
                                 / (exp_for($xlvl+1) - exp_for($xlvl))
                 };

each_iteration
{
    return unless defined $xp;
    $exp_needed = exp_for($xlvl+1) - $xp;
    $exp_needed = 0 if $xlvl == 30;
    $botl{xp} = "Xp:${xlvl}n${exp_needed}";
};

