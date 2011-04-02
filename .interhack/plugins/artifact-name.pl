# this adds three new tabs to help abuse the artifact naming trick
# by Eidolos

make_tab qr/^What do you want to name this gray stone\?/ => "the Heart of Ahriman\n";
make_tab qr/^What do you want to name this \w+ helmet\?/ => "the Mitre of Holiness\n";
make_tab qr/^What do you want to name this \w+ amulet\?/ => "the Eye of the Aethiopica\n";

