# when enabled through #fight, prefixes all movement commands with F, to
# prevent you from wandering off your elbereth tile
# by doy

our $fight_enabled = 0;

sub enable_fight {
    if ($vikeys) {
        remap h => 'Fh';
        remap j => 'Fj';
        remap k => 'Fk';
        remap l => 'Fl';
        remap y => 'Fy';
        remap u => 'Fu';
        remap b => 'Fb';
        remap n => 'Fn';
    }
    else {
        remap 1 => 'F1';
        remap 2 => 'F2';
        remap 3 => 'F3';
        remap 4 => 'F4';
        remap 6 => 'F6';
        remap 7 => 'F7';
        remap 8 => 'F8';
        remap 9 => 'F9';
    }
}

sub disable_fight {
    if ($vikeys) {
        unmap 'h';
        unmap 'j';
        unmap 'k';
        unmap 'l';
        unmap 'y';
        unmap 'u';
        unmap 'b';
        unmap 'n';
    }
    else {
        unmap 1;
        unmap 2;
        unmap 3;
        unmap 4;
        unmap 6;
        unmap 7;
        unmap 8;
        unmap 9;
    }
}

extended_command "#fight"
              => sub
                 {
                   $fight_enabled = not $fight_enabled;
                   if ($fight_enabled) {
                       enable_fight;
                   }
                   else {
                       disable_fight;
                   }
                   "Only Fighting " . ($fight_enabled ? "ON." : "OFF.")
                 };

each_iteration {
    return unless $fight_enabled;

    if ($vt->y == 1) {
        disable_fight;
    }
    else {
        enable_fight;
    }
}
