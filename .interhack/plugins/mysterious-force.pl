# adds a mysterious force counter
# also adds an extended command #hell (#force is taken)
# by Eidolos

our $mysterious_force = 0;

make_annotation "A mysterious force momentarily surrounds you..."
       => sub { "That's force number " . ++$mysterious_force . "." };

extended_command "#hell"
              => sub { $mysterious_force . " mysterious forces." };

