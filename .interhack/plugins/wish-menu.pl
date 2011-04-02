# adds a little menu for easy wishing
# it intentionally doesn't send the \n for you so you can check or back up
# known issues: max of 22 items available (not enforced)
#               screws up the display (mildly fixable)
# by Eidolos

our $wish_quantity = 2 unless defined $wish_quantity;
our $wish_enchantment = 3 unless defined $wish_enchantment;
our $bfg = "blessed fixed greased" unless defined $bfg;
our $armor = "$bfg +$wish_enchantment";

our %wishes =
(
    A => "$wish_quantity blessed scrolls of charging",
    B => "$wish_quantity blessed scrolls of genocide",
    C => "uncursed fixed greased magic marker",

    D => "$armor gray dragon scale mail",
    E => "$armor silver dragon scale mail",
    F => "$armor speed boots",
    G => "$armor jumping boots",
    H => "$armor helm of brilliance",
    I => "$armor helm of telepathy",
    J => "$armor helm of opposite alignment",
    K => "$armor gauntlets of power",

    L => "$bfg ring of conflict",
    M => "$bfg ring of levitation",
    N => "$bfg ring of teleport control",

    O => "$armor Grayswandir",
    P => "$wish_quantity cursed potions of gain level",
    Q => "$bfg amulet of life saving",
    R => "7 $bfg candles",
    S => "$bfg bag of holding",
    T => "$bfg tooled horn",
);

our %law_wishes =
(
    U => "$armor Sceptre of Might",
    V => "$bfg Magic Mirror of Merlin",
    W => "$bfg Orb of Detection",
);

our %neu_wishes =
(
    U => "$bfg Eye of the Aethiopica",
    V => "$bfg Orb of Fate",
    W => "$bfg Platinum Yendorian Express Card",
);

our %cha_wishes =
(
    U => "$bfg Master Key of Thievery",
    V => "$bfg spellbook of identify",
    W => "$bfg spellbook of magic mapping",
);

my $added_align = '';

each_iteration
{
    return if $added_align eq $align;
    my %align_wishes;

       if ($align eq 'Law') { %align_wishes = %law_wishes }
    elsif ($align eq 'Neu') { %align_wishes = %neu_wishes }
    elsif ($align eq 'Cha') { %align_wishes = %cha_wishes }

    while (my ($letter, $wish) = each(%align_wishes))
    {
        $wishes{$letter} = $wish;
    }

    $added_align = $align;
}

show_menu qr/^For what do you wish\? +$/ => \%wishes;

