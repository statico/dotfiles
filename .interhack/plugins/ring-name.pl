# This adds tabs for easy ring naming based on dropping them into sinks
# by Eidolos (idea by toft)

our %sink_message;
my $last_sink_drop = '';

my @ring_sink_regex =
(
  [qr/The faucets flash brightly for a moment\./ => "adornment"],
  [qr/Suddenly, .* vanishes from the sink!/ => "hunger"],
  [qr/The sink glows (?:silver|black) for a moment\./ => "protection"],
  [qr/The sink looks nothing like a fountain\./ => "p from sc"],
  [qr/The sink seems to blend into the floor for a moment/ => "stealth"],
  [qr/The water flow seems fixed/ => "sustain ability"],
  [qr/The sink glows white for a moment/ => "warning"],
  [qr/Several flies buzz angrily around the sink/ => "aggravate monster"],
  [qr/The cold water faucet flashes brightly for a moment/ => "cold res"],
  [qr/The water flow seems (?:greater|lesser) now/ => "gain constitution"],
  [qr/The water flow seems (?:stronger|weaker) now/ => "gain strength"],
  [qr/The water flow (?:hits|misses) the drain/ => "increase accuracy"],
  [qr/The water's force seems (?:greater|smaller) now/ => "increase damage"],
  [qr/You don't see anything happen to the sink/ => "invisibility"],
  [qr/You smell rotten / => "poison resistance"],
  [qr/You see some air in the sink/ => "see invisible"],
  [qr/Static electricity surrounds the sink/ => "shock resistance"],
  [qr/The hot water faucet flashes brightly for a moment/ => "fire res"],
  [qr/You see the ring slide right down the drain!/ => "free action"],
  [qr/The sink quivers upward for a moment/ => "levitation"],
  [qr/The sink looks as good as new/ => "regeneration"],
  [qr/You thought your ring got lost in the sink, / => "searching"],
  [qr/The ring is regurgitated!/ => "slow digestion"],
  [qr/The sink momentarily vanishes/ => "teleportation"],
  [qr/You hear loud noises coming from the drain/ => "conflict"],
  [qr/The sink momentarily looks like a fountain/ => "polymorph"],
  [qr/The sink momentarily looks like a regularly erupting/ => "polycontrol"],
  [qr/The sink looks like it is being beamed aboard somewhere/ => "TC"],
);

each_match qr/You drop an? (?:blessed |(?:un)?cursed )?(\w+(?: \w+)?) ring down the drain\./
    => sub
       {
           $last_sink_drop = $1;
       };
each_match qr/^Call a ([\w\s]+) ring:/
    => sub
       {
           return unless exists $sink_message{$1};
           tab($sink_message{$1}."\n");
       };

each_iteration
{
  return if $last_sink_drop eq '';

  for my $sink (@ring_sink_regex)
  {
    if ($_ =~ $sink->[0])
    {
      $sink_message{$last_sink_drop} = $sink->[1];
      $last_sink_drop = '';
      annotate("That ring is $sink->[1].");
      return;
    }
  }
}

