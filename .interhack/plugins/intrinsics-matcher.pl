include "stats";

my %role_intrinsics =
(
    Arc => ['stealth', 'fast'],
    Bar => ['poison'],
    Cav => [],
    Hea => ['poison'],
    Kni => [],
    Mon => ['fast', 'sleep', 'see invisible'],
    Pri => [],
    Rog => ['searching'],
    Ran => ['stealth'],
    Sam => ['fast'],
    Tou => [],
    Val => ['cold'],
    Wiz => [],
);

my %race_intrinsics =
(
    'Dwa' => [],
    'Elf' => [],
    'Hum' => [],
    'Orc' => ['poison'],
    'Gno' => [],
);
my $seen_role = '';
my $seen_race = '';
my $tracker_path = 'intrinsics-tracker.pl';
my $annotations_path = 'annotations-messages.pl';

# the "or return"s are to avoid making annotations if there was no change
my $add = sub
{
    my $intrinsic = shift;
    my $quiet = shift;
    add_intrinsic($intrinsic) or return if $plugin_loaded{$tracker_path};
    return if $quiet;
    annotate_add_intrinsic($intrinsic) if $plugin_loaded{$annotations_path};
};

my $del = sub
{
    my $intrinsic = shift;
    my $quiet = shift;
    del_intrinsic($intrinsic) or return if $plugin_loaded{$tracker_path};
    return if $quiet;
    annotate_del_intrinsic($intrinsic) if $plugin_loaded{$annotations_path};
};

# Check for starting intrinsics and stuff
each_iteration
{
    if ($seen_role ne $role)
    {
        for my $intrinsic (@{$role_intrinsics{$role}})
        {
            $add->($intrinsic, 1);
        }
        $seen_role = $role;
    }
    if ($seen_race ne $race)
    {
        for my $intrinsic (@{$race_intrinsics{$race}})
        {
            $add->($intrinsic, 1);
        }
        $seen_race = $race;
    }
}

# good messages {{{
# resists {{{
each_match qr/You feel (?:(?:especially )?healthy|hardy)\./ => $add, "poison";
each_match qr/You feel (?:full of hot air|warm)\./ => $add, "cold";
each_match qr/You (?:feel a momentary chill|feel cool(?!er)|be chillin')\./ => $add, "fire";
each_match qr/You feel (?:wide )?awake./ => $add, "sleep";
each_match qr/You feel (?:very firm|totally together, man)\./ => $add, "disintegration";
each_match "Your health currently feels amplified!" => $add, "shock";
each_match qr/You feel (?:insulated|grounded in reality)/ => $add, "shock";
# }}}
# other intrinsics {{{
each_match qr/You feel (?:very jumpy|diffuse)\./ => $add, "teleportitis";
each_match qr/You feel (?:in control of yourself|centered in your personal space)\./ => $add, "teleport control";
each_match "You feel controlled." => $add, "teleport control";
each_match qr/You feel (?:a strange mental acuity|in touch with the cosmos)\./ => $add, "telepathy";
each_match "You feel hidden." => $add, "invisibility";
each_match "You feel perceptive." => $add, "searching";
each_match "You feel stealthy." => $add, "stealth";
each_match "You feel sensitive." => $add, "warning";
each_match qr/You feel (?:very self-conscious|transparent)\./ => $add, "see invisible";
each_match "You see an image of someone stalking you." => $add, "see invisible";
each_match "Your vision becomes clear." => $add, "see invisible";
each_match qr/You (?:seem faster|feel quick|speed up)./ => $add, "fast";
each_match qr/"and thus I grant thee the gift of Speed!"/ => $add, "fast";
# }}}
# notices that we still have a resistance {{{
each_match "You seem unaffected by the poison" => $add, "poison";
each_match "The poison doesn't seem to affect you" => $add, "poison";
each_match "The fire doesn't feel hot!" => $add, "fire";
each_match "The feel mildly hot." => $add, "fire";
each_match "You don't feel cold." => $add, "cold";
each_match "You feel mildly chilly." => $add, "cold";
each_match "You feel a mild tingle." => $add, "shock";
each_match "You seem unhurt." => $add, "shock";
each_match "You are not disintegrated." => $add, "disintegration";
# }}}
# }}}

# dangerous messages {{{
# losing resists {{{
each_match "You feel warmer." => $del, "fire";
each_match "You feel cooler." => $del, "cold";
each_match "You feel a little sick." => $del, "poison";
each_match "You feel tired." => $del, "sleep";
each_match "You feel conductive." => $del, "shock";
# }}}
# losing intrinsics {{{
each_match "You feel unaware." => $del, "warning";
each_match "You slow down." => $del, "fast";
each_match "You seem slower." => $del, "fast";
each_match qr/You feel (?:slow|slower)\./ => $del, "fast";
each_match "You feel paranoid." => $del, "invisibility";
each_match "You feel clumsy." => $del, "stealth";
each_match "You feel less jumpy." => $del, "teleportitis";
each_match "You feel uncontrolled." => $del, "teleport control";
each_match qr/You (?:thought you saw something|tawt you taw a puttie tat)\./ => $add, "see invisible";
each_match "Your senses fail." => $del, "telepathy";
# }}}
# }}}
