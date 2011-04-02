# add extended-commands #write and #notes (need better names.. don't want to use
# #note and #notes)
# by Eidolos

our $note_file ||= 'notes.txt';

sub open_notes # {{{
{
    my $rw = shift || '>>';
    open my $h, $rw, $note_file
        or warn "Unable to open $note_file for ${rw}ing: $!";
    return $h;
} # }}}
my $note_handle = open_notes('>>');

sub make_note # {{{
{
    print {$note_handle} map {"T:$turncount $dlvl $_\n"} @_;
} # }}}
sub note_all # (stuff noted every time, like wishes) # {{{
{
    my ($matching, $note) = @_;
    each_match $matching => sub { make_note(value_of($note)) };
} # }}}
# sub note_once (stuff noted only once for a dlvl, like shops) {{{
{
    my %seen;

    sub note_once
    {
        my ($matching, $note) = @_;

        each_match $matching => sub
        {
            my $t = value_of($note);
            return if $t eq '' || $seen{"$dlvl $t"}++;
            make_note($t);
        }
    }
} # }}}

extended_command "#write" # {{{
              => sub
                 {
                     my (undef, $args) = @_;
                     if (!defined($args))
                     {
                        return "Syntax: #write [note]";
                     }
                     make_note $args;
                     return "Noted!";
                 }; # }}}
extended_command "#notes" # {{{
              => sub
                 {
                     close $note_handle;

                     $ENV{EDITOR} = 'vi' unless exists $ENV{EDITOR};
                     system("$ENV{EDITOR} $note_file");

                     $note_handle = open_notes('>>');
                     request_redraw();

                     return "Thank you sir, may I have another?";
                 }; # }}}

# set up some autonotes (this will become its own plugin) {{{
# major events {{{
note_all qr/^For what do you wish\?/ => "Got a wish!";
note_all qr/Welcome to experience level (\d+)\./ => sub { "Hit experience level $1." };
note_all 'You receive a faint telepathic message from ' => "Quest!";
note_all '"So thou thought thou couldst kill me, fool."' => "Rodney encounter!";
note_all 'Double Trouble...' => "Double Trouble!";
note_all 'A mysterious force momentarily surrounds you...' => "Hit by the mysterious force.";
note_all 'But now thou must face the final Test...' => "Entered the Endgame.";
# }}}
# altars {{{
note_once qr/There is an altar to .*? \((\w+)\) here\./ => sub { "\u$1 altar" };
note_once qr/^.\s+.*?\((\w+) altar\)/ => sub { "\u$1 altar" };
# }}}
# shops {{{
# when we get a generic shop message, we check $shop{dlvl} -- if it's already
# set to >0, then we have seen a more specific shop message
our %shop;

note_once qr/You hear (?:the chime of a cash register|someone cursing shoplifters)\./ => sub { $shop{$dlvl} ? "" : "Level has some kind of shop"};

# get rid of "again" when persistence is in..
our %shoptype =
(
    'general store' => 'general',
    'used armor dealership' => 'armor',
    'second-hand bookstore' => 'scroll',
    'liquor emporium' => 'potion',
    'antique weapons outlet' => 'weapon',
    'delicatessen' => 'food',
    'jewelers' => 'ring',
    'quality apparel and accessories' => 'wand',
    'hardware store' => 'tool',
    'rare books' => 'spellbook',
    'lighting store' => 'lighting',
);
my $shops = join '|', keys %shoptype;

note_once qr/"[^\e]+Welcome(?: again)? to .*? ($shops)!"/
       => sub { $shop{$dlvl} = 1; "Level has a $shoptype{$1} store"};
# }}}
# nonshop special rooms {{{
note_once "You hear the footsteps of a guard on patrol." => "Level has a vault";
# }}}
# }}}

