# for abusing the bug that called scrolls can be written with 100% accuracy
# by Eidolos

recolor qr/scrolls? labeled [A-Z0-9 ]+/ => 'green';

make_tab qr/(.) - (?:an?|\d+)(?: blessed| uncursed| cursed)?(?: fireproof)?(?: greased)? scrolls? labeled ([A-Z0-9 ]+)\./ =>
sub
{
    return "#name\nn$1\U$2\n";
}

