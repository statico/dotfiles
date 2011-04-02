# if you're engrave IDing a wand, you need to have text on the ground before
# you engrave with the wand. therefore, if you're just writing in the dust,
# you're doing it wrong, so we make sure you know about it
# no special handling is added for vanishing wands
# by Eidolos

recolor qr/You write in the dust with a.* wand.*\./ => "red";

