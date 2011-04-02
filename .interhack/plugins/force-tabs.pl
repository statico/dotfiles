# this sets up some "press tab to continue" prompts for important messages,
# like "You are slowing down."
# by Eidolos and toft (idea by doy)

press_tab(qr/\bYou are slowing down\./, "Lizard, !acid, STF, pray? 2 turns to live.");
press_tab(qr/\bYou don't feel very well\./, 'Fire, Cure Sickness, Aesc, "unchang, poly, pray? Around 10 turns to live.');
press_tab qr/\bYou faint from lack of food\./;
press_tab(qr/\bYou feel deathly sick\./, "You have about 20 turns to live.");
press_tab(qr/\bYou feel (?:much|even) worse\./, "Your turns to live have just been cut to about a third.");
press_tab qr/\bStop eating\?/;
press_tab(qr/\bThe [^.!\e]*? swings itself around you!/, 'Kill, tele, poly, "MB, freeze, delev? 1 turn to live.');
press_tab qr/^Really quit\? \[yn\] \(n\) +$/;

press_tab qr/"So thou thought thou couldst kill me, fool\."/;
press_tab qr/\bDouble Trouble\.\.\./;

press_tab qr/\bNothing happens\./, undef, 0.5;
