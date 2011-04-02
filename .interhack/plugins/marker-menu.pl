# this plugin lets you quickly write the kind of scroll/spellbook you want to
# write with a magic marker
# by Eidolos

our %spellbooks =
(
  A => 'force bolt',
  B => 'drain life',
  C => 'magic missile',
  D => 'cone of cold',
  E => 'fireball',
  F => 'finger of death',
  G => 'healing',
  H => 'cure blindness',
  I => 'cure sickness',
  J => 'extra healing',
  K => 'stone to flesh',
  L => 'restore ability',
  M => 'detect monsters',
  N => 'light',
  O => 'detect food',
  P => 'clairvoyance',
  Q => 'detect unseen',
  R => 'identify',
  S => 'detect treasure',
  T => 'magic mapping',
  U => 'sleep',
  V => 'confuse monster',
  W => 'slow monster',
  X => 'cause fear',
  Y => 'charm monster',
  Z => 'protection',
  a => 'create monster',
  b => 'remove curse',
  c => 'create familiar',
  d => 'turn undead',
  e => 'jumping',
  f => 'haste self',
  g => 'invisibility',
  h => 'levitation',
  i => 'teleport away',
  j => 'knock',
  k => 'wizard lock',
  l => 'dig',
  m => 'polymorph',
  n => 'cancellation',
);

our %scrolls =
(
  A => 'charging',
  B => 'enchant armor',
  C => 'enchant weapon',
  D => 'genocide',
  E => 'identify',
  F => 'remove curse',
  G => 'magic mapping',
  H => 'gold detection',
  I => 'taming',
  J => 'scare monster',
  K => 'teleportation',
  L => 'earth',
  M => 'create monster',
  N => 'light',
  O => 'confuse monster',
  P => 'destroy armor',
  Q => 'fire',
  R => 'food detection',
  S => 'amnesia',
  T => 'punishment',
  U => 'stinking cloud',
) unless defined %scrolls;

our %ink_cost =
(
    'mail' => '1',
    'amnesia' => '4-7',
    'earth' => '4-7',
    'fire' => '4-7',
    'gold detection' => '4-7',
    'food detection' => '4-7',
    'light' => '4-7',
    'magic mapping' => '4-7',
    'create monster' => '5-9',
    'destroy armor' => '5-9',
    'punishment' => '5-9',
    'confuse monster' => '6-11',
    'identify' => '7-13',
    'charging' => '8-15',
    'enchant armor' => '8-15',
    'enchant weapon' => '8-15',
    'remove curse' => '8-15',
    'scare monster' => '10-19',
    'stinking cloud' => '10-19',
    'taming' => '10-19',
    'teleportation' => '10-19',
    'genocide' => '15-29',
);

# find longest scroll name...
my $longest = 0;
while (my ($slot, $scroll) = each(%scrolls))
{
    $longest = length($scroll) if length($scroll) > $longest;
}

# so we can use it to have a nice table for ink costs
for my $scroll (values %scrolls)
{
    next unless exists $ink_cost{$scroll};
    $scroll = sprintf "%-${longest}s # %s", $scroll, $ink_cost{$scroll};
}

show_menu qr/^What type of scroll do you want to write\? +$/ => \%scrolls;
#show_menu qr/^What type of spellbook do you want to write\? +$/ => \%spellbooks;

