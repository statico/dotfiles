# adds a bunch of annotations for the intrinsics you can get from various
# corpses
# by Eidolos (idea by toft)

# probably should add nutrition too

my %annotation_for =
(
    "abbots?"                   => "Hallucination",
    "acid blobs?"               => "Acidic",
    "baby green dragons?"       => "Poisonous",
    "baby yellow dragons?"      => "Acidic",
    "bats?"                     => "Stun (30 turns)",
    "black dragons?"            => "Disintegration: 100%",
    "black nagas?"              => "Acidic, Poison: 53%",
    "black naga hatchlings?"    => "Acidic, Poison: 20%",
    "black puddings?"           => "Acidic, Cold: 22%, Shock: 22%, Poison: 22%",
    "black unicorns?"           => "Poison: 27%",
    "blue dragons?"             => "Shock: 100%",
    "blue jell(?:y|ies)"        => "Cold: 13%, Poison: 13%",
    "brown molds?"              => "Cold: 3%, Poison: 3%",
    "brown puddings?"           => "Acidic, Cold: 11%, Shock: 11%, Poison: 11%",
    "cave spiders?"             => "Poison: 7%",
    "centipedes?"               => "Poison: 13%",
    "chameleons?"               => "Polymorph",
    "chickatrices?"             => "Petrify, Poison: 27%",
    "Chromatic Dragon"          => "Poisonous, Fire: 17%, Cold: 17%, Sleep: 17%, Shock: 17%, Poison: 17%, Disintegration: 17%",
    "cobras?"                   => "Poisonous, Poison: 40%",
    "cockatrices?"              => "Petrify, Poison: 33%",
    "Cyclops"                   => "Strength: 100%",
    "Death"                     => "DIE",
    "dogs?"                     => "Aggravate",
    "doppelgangers?"            => "Polymorph",
    "electric eels?"            => "Shock: 47%",
    "el(?:f|ves)"               => "Sleep: 67%",
    "elf-lords?"                => "Sleep: 53%",
    "Elvenkings?"               => "Sleep: 60%",
    "Famine"                    => "DIE",
    "fire ants?"                => "Fire: 20%",
    "fire giants?"              => "Strength: 100%, Fire: 30%",
    "flesh golems?"             => "Fire: 12%, Cold: 12%, Sleep: 12%, Shock: 12%, Poison: 12%",
    "floating eyes?"            => "Telepathy: 100%",
    "frost giants?"             => "Strength: 100%, Cold: 33%",
    "gelatinous cubes?"         => "Acidic, Fire: 10%, Cold: 10%, Sleep: 10%, Shock: 10%",
    "giants?"                   => "Strength: 100%",
    "giant bats?"               => "Stun (60 turns)",
    "giant beetles?"            => "Poisonous, Poison: 33%",
    "giant mimics?"             => "Mimic (50 turns)",
    "giant spiders?"            => "Poisonous, Poison: 33%",
    "golden nagas?"             => "Poison: 67%",
    "golden naga hatchlings?"   => "Poison: 20%",
    "gray oozes?"               => "Acidic, Fire: 7%, Cold: 7%, Poison: 7%",
    "gray unicorns?"            => "Poison: 27%",
    "green dragons?"            => "Poisonous, Poison: 100%",
    "green molds?"              => "Acidic",
    "green slimes?"             => "Slime, Poisonous, Acidic",
    "Green-el(?:f|ves)"         => "Sleep: 33%",
    "gremlins?"                 => "Poisonous, Poison: 33%",
    "Grey-el(?:f|ves)"          => "Sleep: 40%",
    "guardian nagas?"           => "Poisonous, Poison: 80%",
    "guardian naga hatchlings?" => "Poison: 20%",
    "hell hounds?"              => "Fire: 80%",
    "hell hound pups?"          => "Fire: 47%",
    "hill giants?"              => "Strength: 100%",
    "homuncul(?:us|i)"          => "Poisonous, Sleep: 7%, Poison: 7%",
    "housecats?"                => "Aggravate",
    "ice trolls?"               => "Cold: 60%",
    "Ixoth"                     => "Fire: 100%",
    "jellyfish"                 => "Poisonous, Poison: 20%",
    "killer bees?"              => "Poisonous, Poison: 30%",
    "kittens?"                  => "Aggravate",
    "kobolds?"                  => "Poisonous",
    "kobold lords?"             => "Poisonous",
    "kobold shamans?"           => "Poisonous",
    "large cats?"               => "Aggravate",
    "large dogs?"               => "Aggravate",
    "large kobolds?"            => "Poisonous",
    "large mimics?"             => "Mimic (40 turns)",
    "leprechauns?"              => "Teleportitis: 50%",
    "little dogs?"              => "Aggravate",
    "lizards?"                  => "Cure stoning: 100%, Reduce stunning; confusion: 100%",
    "Lord Surtur"               => "Strength: 100%, Fire: 50%",
    "Master Kaen"               => "Poison: 100%",
    "master mind flayers?"      => "Intelligence: 50%, Int Gain / Telepathy: 100%",
    "Medusa"                    => "Petrify, Poisonous, Poison: 100%",
    "mind flayers?"             => "Intelligence: 50%, Int Gain / Telepathy: 100%",
    "mountain nymphs?"          => "Teleportitis: 30%",
    "newts?"                    => "Energy: 67%",
    "nurses?"                   => "Heal: 100%, Poison: 73%",
    "ochre jellys?"             => "Acidic",
    "orange dragons?"           => "Sleep: 100%",
    "Pestilence"                => "DIE",
    "pit vipers?"               => "Poisonous, Poison: 40%",
    "pyrolisks?"                => "Fire: 20%, Poison: 20%",
    "quantum mechanics?"        => "Poisonous, Speed Toggle: 100%",
    "quasits?"                  => "Poison: 20%",
    "queen bees?"               => "Poisonous, Poison: 60%",
    "quivering blobs?"          => "Poison: 33%",
    "rabid rats?"               => "Poisonous",
    "red dragons?"              => "Fire: 100%",
    "red molds?"                => "Fire: 3%, Poison: 3%",
    "red nagas?"                => "Fire: 20%, Poison: 20%",
    "red naga hatchlings?"      => "Fire: 10%, Poison: 10%",
    "salamanders?"              => "Poisonous, Fire: 53%",
    "scorpions?"                => "Poisonous, Poison: 50%",
    "Scorpius"                  => "Poisonous, Poison: 100%",
    "shriekers?"                => "Poison: 20%",
    "small mimics?"             => "Mimic (20 turns)",
    "snakes?"                   => "Poisonous, Poison: 27%",
    "soldier ants?"             => "Poisonous, Poison: 20%",
    "spotted jell(?:y|ies)"     => "Acidic",
    "stalkers?"                 => "Stun (60 turns), Invisibility: 100%",
    "stone giants?"             => "Strength: 100%",
    "storm giants?"             => "Strength: 100%, Shock: 50%",
    "tengu"                     => "Poison: 13%, Teleportitis: 20%, Teleport control: 17%",
    "vampire bats?"             => "Poisonous",
    "violet fung(?:us|i)"       => "Hallucination, Poison: 20%",
    "water moccasins?"          => "Poisonous, Poison: 27%",
    "water nymphs?"             => "Teleportitis: 30%",
    "werejackals?"              => "Poisonous, Lycanthropy: 100%",
    "wererats?"                 => "Poisonous, Lycanthropy: 100%",
    "werewol(?:f|ves)?"         => "Poisonous, Lycanthropy: 100%",
    "white dragons?"            => "Cold: 100%",
    "white unicorns?"           => "Poison: 27%",
    "winter wolfs?"             => "Cold: 47%",
    "winter wolf cubs?"         => "Cold: 33%",
    "Wizard of Yendor"          => "Fire: 25%, Poison: 25%, Teleportitis: 25%, Teleport control: 25%",
    "wood nymphs?"              => "Teleportitis: 30%",
    "Woodland-el(?:f|ves)?"     => "Sleep: 27%",
    "wraiths?"                  => "Gain level: ?%",
    "xans?"                     => "Poisonous, Poison: 47%",
    "yellow dragons?"           => "Acidic",
    "yellow molds?"             => "Poisonous, Hallucination, Poison: 7%",
    "yetis?"                    => "Cold: 33%",
);

my %cannibalism =
(
    Hum =>
         {
            'abbots?' => 1,
            'acolytes?' => 1,
            'priests?' => 1,
            'priestess(?:es)?' => 1,
            'apprentices?' => 1,
            'Arch Priest' => 1,
            'archeologists?' => 1,
            'attendants?' => 1,
            'barbarians?' => 1,
            'captains?' => 1,
            'cavem[ae]n' => 1,
            'cavewom[ae]n' => 1,
            'chieftains?' => 1,
            'Croesus' => 1,
            'doppelgangers?' => 1,
            'Grand Master' => 1,
            'guards?' => 1,
            'guides?' => 1,
            'healers?' => 1,
            'high priests?' => 1,
            'Hippocrates' => 1,
            'humans?' => 1,
            'hunters?' => 1,
            'Keystone Kops?' => 1,
            'Kop Lieutenants?' => 1,
            'Kop Sergeants?' => 1,
            'Kop Kaptains?' => 1,
            'King Arthur' => 1,
            'knights?' => 1,
            'lieutenants?' => 1,
            'Lord Carnarvon' => 1,
            'Lord Sato' => 1,
            'Master Assassin' => 1,
            'Master Kaen' => 1,
            'Master of Thieves' => 1,
            'monks?' => 1,
            'neanderthals?' => 1,
            'Neferet the Green' => 1,
            'ninjas?' => 1,
            'Norn' => 1,
            'nurses?' => 1,
            'Oracle' => 1,
            'Orion' => 1,
            'pages?' => 1,
            'Pelias' => 1,
            'prisoners?' => 1,
            'rangers?' => 1,
            'rogues?' => 1,
            'roshis?' => 1,
            'samurais?' => 1,
            'sergeants?' => 1,
            'Shaman Karnov' => 1,
            'shopkeepers?' => 1,
            'soldiers?' => 1,
            'students?' => 1,
            'thugs?' => 1,
            'tourists?' => 1,
            'Twoflower' => 1,
            'valkyries?' => 1,
            'warriors?' => 1,
            'watch captains?' => 1,
            'watchm[ae]n' => 1,
            'werejackals?' => 1,
            'wererats?' => 1,
            'werewolfs?' => 1,
            'wizards?' => 1,
            'Wizard of Yendor' => 1,
         },
    Elf =>
         {
            'el(?:f|ves)' => 1,
            'elf-lords?' => 1,
            'Elvenkings?' => 1,
            'Green-el(?:f|ves)' => 1,
            'Grey-el(?:f|ves)' => 1,
            'Woodland-el(?:f|ves)' => 1,
         },
    Dwa =>
         {
            'dwar(?:f|ves)' => 1,
            'dwarf lords?' => 1,
            'dwarf kings?' => 1,
         },
    Gno =>
         {
            'gnomes?' => 1,
            'gnome lords?' => 1,
            'gnome kings?' => 1,
            'gnomish wizards?' => 1,
         },
    Orc => {}, # orcs don't suffer from cannibalism
);

my $helper = sub
{
    my ($monster, $intrinsics) = @_;

    my @int = split ',', $intrinsics;
    for (@int) {
        if (/: \S+%/) {
            $_ = "\e[1;32m$_\e[1;30m";
        } else {
            $_ = "\e[1;31m$_\e[1;30m";
        }
    }
    $intrinsics = join ',', @int;

    make_annotation qr/^You see here (?:an? (?:partly eaten )?)?$monster(?:'s?)? corpse\./ => "Corpse: $intrinsics";
    make_annotation qr/^There is (?:an? (?:partly eaten )?)?$monster(?:'s?)? corpse here; eat it\?/ => "Corpse: $intrinsics";
    make_annotation qr/It smells like $monster\./ => "Tin: $intrinsics";
};

while (my ($monster, $intrinsics) = each %annotation_for)
{
    $helper->($monster, $intrinsics);
}

my $seen_race = '';
each_iteration
{
    return if $race eq $seen_race;
    $seen_race = $race;

    # it's currently okay if we kind of supercede an existing annotation
    # since annotations are displayed in the order they were initialized
    # this will require fixing when we get multiple-annotations-in-one-move
    # working.. :)

    while (my ($monster, undef) = each %{$cannibalism{$race}})
    {
        my $intrinsics = 'Cannibalism';
        $intrinsics .= ", $annotation_for{$monster}" if exists $annotation_for{$monster};
        $helper->($monster, $intrinsics);
    }
}

