# add a tab for Elbereth when you're writing stuff on the ground
# by Eidolos

make_tab qr/^What do you want to (?:write|engrave|burn)(?! (?:wi|on))/ => "Elbereth\n";

make_tab qr/^What do you want to scribble on / => "Elbereth\n";

make_tab qr/^What do you want to add to the (?:writing|engraving|text burned|graffiti)/ => "Elbereth\n";

