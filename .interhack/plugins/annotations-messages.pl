my %add_annotation_for =
(
    cold => "Gained cold resistance.",
    disintegration => "Gained disintegration resistance.",
    fast => "You are now 'fast'.",
    fire => "Gained fire resistance.",
    invisible => "Gained invisibility.",
    poison => "Gained poison resistance.",
    searching => "Gained searching intrinsic.",
    'see invisible' => "Gained see invisible.",
    shock => "Gained shock resistance.",
    sleep => "Gained sleep resistance.",
    stealth => "Gained stealth intrinsic.",
    'teleport control' => "Gained teleport control.",
    telepathy => "Gained intrinsic telepathy.",
    teleportitis => "Gained teleportitis.",
    warning => "Gained warning intrinsic.",
);

my %del_annotation_for =
(
    cold => "Lost cold resistance.",
    fast => "You are no longer 'fast'.",
    fire => "Lost fire resistance.",
    invisibility => "Lost invisibility intrinsic.",
    poison => "Lost poison resistance.",
    'see invisible' => "Lost see invisible.",
    shock => "Lost shock resistance",
    sleep => "Lost sleep resistance.",
    stealth => "Lost stealth intrinsic.",
    'teleport control' => "Lost teleport control.",
    telepathy => "Lost intrinsic telepathy.",
    teleportitis => "Lost teleportitis.",
);

sub annotate_add_intrinsic
{
    my $intrinsic = shift;
    annotate($add_annotation_for{$intrinsic}) if $add_annotation_for{$intrinsic};

}

sub annotate_del_intrinsic
{
    my $intrinsic = shift;
    annotate($del_annotation_for{$intrinsic}) if $del_annotation_for{$intrinsic};

}


make_annotation 'You feel a mild buzz.' => "Gained 1-3 energy.";
make_annotation 'You feel less attractive.' => sub{"Lost aggravate monster intrinsic."};
make_annotation 'You feel vulnerable.' => sub{"Lost protection."};


make_annotation qr/The .*? yowls!/ => "Your pet is hungry!";
make_annotation 'Oh wow!  Great stuff!' => "You are now hallucinating.";
make_annotation 'You reel...' => "You are now stunned.";
make_annotation 'You feel somewhat dizzy.' => "You are now confused.";
make_annotation 'You feel feverish.' => "You are now a werefoo.";

