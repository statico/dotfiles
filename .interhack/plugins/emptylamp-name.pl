# Adds a tab that names lamps empty appropriately
# by toft

our $empty ||= "empty";

make_tab qr/^This (?:oil )?lamp.*? has no oil\./
    => sub{my $key = alphakeys(-1); " \e#name\ny${key}$empty\n"};
make_tab qr/^Your lamp.*? has run out of power\./
    => sub{my $key = alphakeys(-1); " \e#name\ny${key}$empty\n"};
