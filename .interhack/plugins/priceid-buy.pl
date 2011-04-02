our %short_names;
our %price_table =
(
  'scroll' => {
              20  => ['identify'],
              50  => ['light'],
              60  => ['enchant weapon'],
              80  => ['enchant armor', 'remove curse'],
              100 => ['confusion', 'destroy armor', 'fire',
                      'food detection', 'gold detection', 'magic mapping',
                      'scare monster', 'teleportation'],
              200 => ['amnesia', 'create monster', 'earth', 'taming'],
              300 => ['charging', 'genocide', 'punishment',
                      'stinking cloud'],
          },
  'spellbook' => {
              100 => ['detect monsters', 'force bolt', 'healing', 'jumping',
                      'knock', 'light', 'protection', 'sleep'],
              200 => ['confuse monster', 'create monster', 'cure blindness',
                      'detect food', 'drain life', 'magic missile',
                      'slow monster', 'wizard lock'],
              300 => ['cause fear', 'charm monster', 'clairvoyance',
                      'cure sickness', 'detect unseen', 'extra healing',
                      'haste self', 'identify', 'remove curse',
                      'stone to flesh'],
              400 => ['cone of cold', 'detect treasure', 'fireball',
                      'invisibility', 'levitation', 'restore ability'],
              500 => ['dig', 'magic mapping'],
              600 => ['create familiar', 'polymorph', 'teleport away',
                      'turn undead'],
              700 => ['cancellation', 'finger of death'],
          },
  'potion' => {
              0   => ['water'],
              50  => ['booze', 'fruit juice', 'see invisible', 'sickness'],
              100 => ['confusion', 'extra healing', 'hallucination',
                      'healing', 'restore ability', 'sleeping', 'water'],
              150 => ['blindness', 'gain energy', 'invisibility',
                      'monster detection', 'object detection'],
              200 => ['enlightenment', 'full healing', 'levitation',
                      'polymorph', 'speed'],
              250 => ['acid', 'oil'],
              300 => ['gain ability', 'gain level', 'paralysis'],
          },
  'ring' => {
              100 => ['adornment', 'hunger', 'protection',
                      'protection from shape changers', 'stealth',
                      'sustain ability', 'warning'],
              150 => ['aggravate monster', 'cold resistance',
                      'gain constitution', 'gain strength',
                      'increase accuracy', 'increase damage',
                      'invisibility', 'poison resistance',
                      'see invisible', 'shock resistance'],
              200 => ['fire resistance', 'free action', 'levitation',
                      'regeneration', 'searching', 'slow digestion',
                      'teleportation'],
              300 => ['conflict', 'polymorph', 'polymorph control',
                      'teleport control'],
          },
  'wand' => {
              0   => ['uncharged'],
              100 => ['light', 'nothing'],
              150 => ['digging', 'enlightenment', 'locking',
                      'magic missile', 'make invisible', 'opening',
                      'probing', 'secret door detection', 'slow monster',
                      'speed monster', 'striking', 'undead turning'],
              175 => ['cold', 'fire', 'lightning', 'sleep'],
              200 => ['cancellation', 'create monster', 'polymorph',
                      'teleportation'],
              500 => ['death', 'wishing'],
          },
);

our $shirt = 0;

sub calc_base
{
    my $charisma = shift;
    my $amount = shift;

    if ($charisma > 18)
    {
      $amount *= 2;
    }
    elsif ($charisma > 17)
    {
      $amount += $amount / 2;
    }
    elsif ($charisma > 15)
    {
      $amount += $amount / 3;
    }
    elsif ($charisma < 6)
    {
      $amount /= 2;
    }
    elsif ($charisma < 8)
    {
      $amount -= $amount / 3;
    }
    elsif ($charisma < 11)
    {
      $amount -= $amount / 4;
    }

    if ($amount <= 0)
    {
      $amount = 1;
    }

    $amount -= $amount / 4 if ($role eq 'Tou' && $xlvl <= 15) || $shirt;
    $amount = 0 if $amount == 5;

    $amount_sur = $amount - $amount / 4;
    $amount_sur = 0 if $amount_sur == 5;

    return map {int} ($amount, $amount_sur);
}

make_tab qr/^(?:You have a little trouble lifting )?(.) - (an?|\d+) (?:clear potions?|potions? (?:of|called) water) \(unpaid, (\d+) zorkmids?\)\./
      => sub
         {
             my ($letter, $count, $cost) = ($1, $2, $3);
             $cost /= $count if $count > 0;

             my $buc = 'uncursed';
             if ($cost > 20)
             {
                $buc = '!UC';
             }

             "\e\e#name\ny$letter$buc\n";
         };

make_tab qr/^(?:You have a little trouble lifting )?(.) - (an?|\d+) (?:blessed |uncursed |cursed )?(.*?) ?(scroll|potion|wand|ring|spellbook)s? (?:(?:called |labeled )(.*?) ?)?\(unpaid, (\d+) zorkmids?\)\./
      => sub
         {
             my ($letter, $count, $appearance, $type, $existing, $cost)
              = (     $1,     $2,          $3,    $4,        $5,   $6);

             return if $appearance =~ /\bwater\b/i
                    || $appearance =~ /\bclear\b/i
                    || $appearance =~ /\bunlabeled\b/i
                    || $appearance =~ /\bblank\b/i;

             $existing = "" unless defined $existing;

             # avoid making a tab for existing prices
             return "" if $existing =~ /\b(?<!NR )\d+\b/;

             $count = 1 unless $count =~ /\d/;
             return "" if $count == 0; # just in case!
             $cost /= $count;

             if ($type eq 'scroll' && $appearance eq '')
             {
                 # heuristic: scroll names will have no upper case characters
                 # if they are entirely identified (scroll called identify)
                 return "" if $existing !~ /[A-Z]/;

                 $appearance = $lc_scrolls ? lc $existing : $existing;
                 $existing = '';
             }
             $existing = " $existing" if $existing ne '';

             my ($price1, $price2) = calc_base($ch, $cost);
             my $possibilities = 0;

             for ($price1, map {$price1 - $_, $price1 + $_} 1..4)
             {
                 if (exists $price_table{$type}{$_})
                 {
                     $price1 = $_;
                     $possibilities += @{$price_table{$type}{$_}};
                     last;
                 }
             }

             for ($price2, map {$price2 - $_, $price2 + $_} 1..4)
             {
                 if (exists $price_table{$type}{$_})
                 {
                     $price2 = $_;
                     $possibilities += @{$price_table{$type}{$_}};
                     last;
                 }
             }

             my @possibilities;
             if ($possibilities == 1)
             {
                @possibilities = map {@{$price_table{$type}{$_}}}
                                 $price1, $price2;
             }
             else
             {
               @possibilities = map {@{
                                         $short_names{$type}{$_}
                                      || $price_table{$type}{$_}
                                     }}
                                 $price1, $price2;
             }

             $cost = join '/',
                     grep {exists $price_table{$type}{$_}}
                     sort {$a <=> $b} $price1, $price2;
             return '' if $cost eq '';

             $cost =~ s/(?<=\d)\d+//g
               if $type eq 'spellbook';

             if (@possibilities == 1)
             {
               "\e\e#name\nn$letter$possibilities[0]\n";
             }
             elsif (@possibilities <= 3)
             {
               "\e\e#name\nn$letter$appearance ".(join '/', @possibilities)."\n";
             }
             else
             {
               "\e\e#name\nn$letter$appearance$existing $cost\n";
             }
         };

extended_command "shirt" => sub { $shirt = !$shirt; "You are now marked as " . ($shirt ? "" : "not ") . " wearing a visible shirt." }

