# parse all kinds of info about your character (score, stats, align, etc) so
# that other modules have access to this information
# also clears the penultimate line to allow for other information to be placed
# there
# by Eidolos and doy

our $st = 0;
our $dx = 0;
our $co = 0;
our $in = 0;
our $wi = 0;
our $ch = 0;
our $name = '';
our $align = '';
our $sex = '';
our $race = '';
our $role = '';
our $dlvl = '';
our $au = 0;
our $curhp = 0;
our $maxhp = 0;
our $curpw = 0;
our $maxpw = 0;
our $ac = 0;
our $xlvl = 0;
our $xp = 0;
our $turncount = 0;
our $status = '';

my %aligns = (lawful  => 'Law',
              neutral => 'Neu',
              chaotic => 'Cha',
);

my %sexes = (male   => 'Mal',
             female => 'Fem',
);

my %races = ('dwarven' => 'Dwa',
             'elven'   => 'Elf',
             'human'   => 'Hum',
             'orcish'  => 'Orc',
             'gnomish' => 'Gno',
);

my %roles = (Archeologist => 'Arc',
             Barbarian    => 'Bar',
             Caveman      => 'Cav',
             Cavewoman    => 'Cav',
             Healer       => 'Hea',
             Knight       => 'Kni',
             Monk         => 'Mon',
             Priest       => 'Pri',
             Priestess    => 'Pri',
             Rogue        => 'Rog',
             Ranger       => 'Ran',
             Samurai      => 'Sam',
             Tourist      => 'Tou',
             Valkyrie     => 'Val',
             Wizard       => 'Wiz',
);

# figure out role, race, gender, align
each_match qr/^\w+ (?:\w+ )?(\w+), welcome to NetHack!  You are a (\w+) (\w+) (\w+)(?: (\w+))?\./
    => sub
       {
           if (!defined($5)) {
               ($name, $align, $race, $role) = ($1, $aligns{$2}, $races{$3}, $roles{$4});
               $sex = $4 =~ /(?:woman|ess)$/ ? "Fem" : "Mal";
           }
           else {
               $sex = $sexes{$3};
               ($name, $align, $race, $role) = ($1, $aligns{$2}, $races{$4}, $roles{$5})
           }
       };
each_match qr/^\w+ (?:\w+ )?(\w+), the (\w+) (\w+), welcome back to NetHack!/
    => sub
       {
           ($name, $race, $role) = ($1, $races{$2}, $roles{$3});
           $sex = "Fem" if $3 eq "Cavewoman" || $3 eq "Priestess";
           $sex = "Mal" if $3 eq "Caveman" || $3 eq "Priest";
       };

# figure out stats (strength, score, etc)
each_iteration
{
    my @groups = $vt->row_plaintext(23) =~ /^(\w+)?.*?St:(\d+(?:\/(?:\*\*|\d+))?) Dx:(\d+) Co:(\d+) In:(\d+) Wi:(\d+) Ch:(\d+)\s*(\w+)(?:\s*S:(\d+))?/;
    $show_sl = @groups;
    return if @groups == 0;
    ($st, $dx, $co, $in, $wi, $ch, $align, $score) = @groups[1..8];
    $name = $groups[0] if $groups[0];
    $align = $aligns{lc $align};

    $botl{char} = sprintf "%s: %s%s%s%s", $name,
                                         $role  ? "$role "  : "",
                                         $race  ? "$race "  : "",
                                         $sex   ? "$sex "   : "",
                                         $align ? "$align" : "";
    $botl{stats} = "St:$st Dx:$dx Co:$co In:$in Wi:$wi Ch:$ch";
    $botl{score} = defined($groups[8]) ? "S:$score" : "";
}

# parse botl
each_iteration
{
    my @groups = $vt->row_plaintext(24) =~ /^(Dlvl:\d+|Home \d+|Fort Ludios|End Game|Astral Plane)\s+(?:\$|\*):(\d+)\s+HP:(\d+)\((\d+)\)\s+Pw:(\d+)\((\d+)\)\s+AC:([0-9-]+)\s+(?:Exp|Xp|HD):(\d+)(?:\/(\d+))?(?:\s+T:(\d+))?\s+(.*?)\s*$/;
    $show_bl = @groups;
    return if @groups == 0;
    ($dlvl, $au, $curhp, $maxhp, $curpw, $maxpw, $ac, $xlvl, $xp, $turncount, $status) = @groups;

    $botl{dlvl} = $dlvl;
    $botl{au} = "\$:$au";
    $botl{hp} = "HP:$curhp($maxhp)";
    $botl{pw} = "Pw:$curpw($maxpw)";
    $botl{ac} = "AC:$ac";
    $botl{xp} = sprintf "Xp:$xlvl%s", $xp ? "/$xp" : "";
    $botl{turncount} = "T:$turncount";
    $botl{status} = "$status";
}

each_iteration
{
    my $blocking = 0;
    my ($row,$col) = (0,0); # $col isn't used at this time
    # XXX: you know.. this code doesn't really work right if only one of the 
    # following options is enabled
    return unless $show_sl or $show_bl;

    my $replacement = '';
    $_ =~ s/\x00//g; # strip null bytes as they appear to do nothing
    my @pieces = split /(\e\[[0-9;]*H|\e\[A|\x0d)/;
    while ( scalar(@pieces) ) {
        my $piece = shift @pieces;

        if ( $piece =~ /(\e\[A)/ ) {
            # matched a move up cursor sequence
            $row--;
            $replacement .= $1;
            next;
        }

        if ( $piece =~ /\x0d/ ) {
            # matched a carriage return
            $replacement .= "\x0d";
            next;
        }

        unless ( $piece =~ /\e\[(?:([0-9]+);)?([0-9]*)H/ ) {
            # whee, hacks. don't know what's wrong, so let's see if we can
            # just avoid the issue
            if ( $row >= 23 && ($vt->row_plaintext(23) . $vt->row_plaintext(24)) !~ /(?:\((?:\d+ of \d+|end)\)|--More--)/ ) {
                # we don't want things overwriting our bottom lines so 
                # we replace printing characters with cursor move commands
                # so everything else printed on other lines work

                # move cursor forward by the length of the text we skipped
                # \e[0C actually moves it forward one so check for that too
                my $textlen = length( $piece );
                $replacement .= "\e\[${textlen}C" if $textlen;
            } else {
                $replacement .= $piece;
            }
        } else {
            # matched a full cursor move escape sequence
            ( $row, $col ) = ( $1, $2 );
            $replacement .= $piece;
        }
    }
    $_ = $replacement;
}
