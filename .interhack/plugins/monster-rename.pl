# this is a game. games should be fun. goofy monster names are fun.
# by Eidolos

each_iteration
{
    my $roll = 1 + int rand 20;

    s{rope golem}
     {rape golem}g;

    s{leocrotta}
     {mumakrota}g if $roll > 15;

    s{rust monster}
     {AC rapist}g if $roll > 15;

    s{The Wizard of Yendor}
     {Christian Bressler}ig if $roll == 20;

    s{Wizard of Yendor}
     {Wizard of Oz}g if $roll > 15;

    s{master mind flayer}
     {watch out for :E}g if $roll > 15;
}

