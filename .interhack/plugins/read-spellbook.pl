# adds a new extended command #read which tells you the odds of successfully
# reading spellbooks of different levels
# DEPENDS ON: stats (for $in and $xlvl)
# by Eidolos

sub _calc_read_chance
{
  my ($in, $xl, $sl) = @_;
  my $chance = ($in + 4 + int($xl/2) - 2*$sl);

     if ($chance <  0) { $chance =  0 }
  elsif ($chance > 20) { $chance = 20 }
  return $chance * 5;
}

sub calc_read_chances
{
    my ($in, $xl) = @_;
    my @sl = 1..7;

    join ', ', map { "$_:" . _calc_read_chance($in, $xl, $_) . "%"} @sl;
}

extended_command "#read"
              => sub
                 {
                   calc_read_chances($in, $xlvl, undef);
                 };

