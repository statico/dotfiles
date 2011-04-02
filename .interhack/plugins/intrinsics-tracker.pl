our %intrinsics;

our @intrinsics_colors =
(
    [fast           => "\e[1;35m"],
    [poison         => "\e[0;32m"],
    [sleep          => "\e[1;31m"],
    [fire           => "\e[1;33m"],
    [cold           => "\e[1;36m"],
    [shock          => "\e[1;34m"],
);

our %shorten_intrinsic =
(
    disintegration => 'disint',
    sleep => 'slp',
    shock => 'shk',
    poison => 'pois',
    'see invisible' => 'SI',
    invisibility => 'invis',
    'teleport control' => 'TC',
    teleportitis => 'tpitis',
    searching => 'srch',
    stealth => 'stlth',
    warning => 'warn',
    telepathy => 'ESP',

);

sub show_intrinsics
{
    my %t = %intrinsics; # save another copy so we can delete what we process
    my @intrinsics;

    for my $int_color (@intrinsics_colors)
    {
        my ($intrinsic, $color) = @$int_color;

        my $out = "\e[1;30m";
        $out .= "$color!" unless delete $t{$intrinsic};
        $intrinsic = $shorten_intrinsic{$intrinsic} || $intrinsic;
        $out .= "$intrinsic\e[m";

        push @intrinsics, $out;
    }

    push @intrinsics, sort
                      map { $shorten_intrinsic{$_} || $_ }
                      keys %t;

    return join ' ', @intrinsics;
}

extended_command "#intrinsics" => \&show_intrinsics;
extended_command "#int" => \&show_intrinsics;

# add_intrinsic and del_intrinsic return 0 if no change was made, 1 if there was
# this way you can avoid reminding the player he "still" has poison resistance

sub add_intrinsic
{
    my $intrinsic = shift;
    return 0 if $intrinsics{$intrinsic};
    $intrinsics{$intrinsic} = 1;
    return 1;
}

sub del_intrinsic
{
    my $intrinsic = shift;
    return 0 if !exists($intrinsics{$intrinsic});
    delete $intrinsics{$intrinsic};
    return 1;
}
