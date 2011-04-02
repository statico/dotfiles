# this is essentially NAO's eidocolors with a number of improvements
# it aims to be very configurable, so you can do things like:
#   $ece_gold = 1; # to exclude coloring "gold pieces"
#   $ec_bohboom = "magenta"; # color items like bag of tricks, /oCanc purple
# you have to make these changes BEFORE you include this plugin
# PLEASE PLEASE PLEASE talk to Eidolos before you change any defaults
# I'm fine with anyone else adding new colors as long as they make it OFF
# by default. See doy's uncursed water colors for an example.

# common regexes {{{
my $called = qr/called|of/;
my $colors = qr/(?:\e\[[0-9;]*m)?/;
# }}}
# BUC {{{
if (!$ece_buc)
{
# !C !B !UC {{{
    if (!$ece_shortbuc)
    {
        recolor qr/(?<=named )!C\b/i => $ec_semiknown || $ec_notcursed || "brown";
        recolor qr/(?<=named )!B\b/i => $ec_semiknown || $ec_notblessed || "brown";
        recolor qr/(?<=named )!UC\b/i => $ec_semiknown || $ec_notuncursed || "brown";
    }
# }}}
# blessed uncursed cursed {{{
    if (!$ece_longbuc)
    {
        recolor qr/\buncursed\b|(?<=named )UC?\b/i => $ec_uncursed || "brown";
        recolor qr/\bblessed\b|(?<=named )B\b/i => $ec_blessed || "cyan";
        recolor qr/\bcursed\b|(?<=named )C\b/i => $ec_cursed || "red";
    }
# }}}
}
# }}}
# erosion {{{
recolor qr/(?:very|thoroughly)? ?(?:rusty|burnt|rotted|corroded)/ => $ec_erosion || "red" if $eci_erosion;
# }}}
# water sports {{{
if (!$ece_water)
{
# holy water {{{
    if (!$ece_holywater)
    {
        recolor qr/\bholy water\b/ => $ec_holywater || "bcyan";
        recolor qr/\bblessed clear potions?\b/ => $ec_holywater || "bcyan";
        recolor qr/\bblessed potions? called water\b/ => $ec_holywater || "bcyan";
        recolor qr/\bclear potions? named \b(?:holy|blessed|B)\b/ => $ec_holywater || "bcyan";
        recolor qr/\bpotions? of water named \b(?:holy|blessed|B)\b/ => $ec_holywater || "bcyan";
        recolor qr/\bpotions? called water named \b(holy|blessed|B)\b/ => $ec_holywater || "bcyan";
    }
# }}}
# unholy water {{{
    if (!$ece_unholywater)
    {
        recolor qr/\bunholy water\b/ => $ec_unholywater || "orange";
        recolor qr/\bcursed clear potions?\b/ => $ec_unholywater || "orange";
        recolor qr/\bcursed potions? called water\b/ => $ec_unholywater || "orange";
        recolor qr/\bclear potions? named \b(?:unholy|cursed|C)\b/ => $ec_unholywater || "orange";
        recolor qr/\bpotions? of water named \b(?:unholy|cursed|C)\b/ => $ec_unholywater || "orange";
        recolor qr/\bpotions? called water named (?:unholy|cursed|C)\b/ => $ec_unholywater || "orange";
    }
# }}}
# split water coloring {{{
    if ($eci_splitwater)
    {
        recolor qr/\bclear potions?\b/ => $ec_water || "cyan";
        recolor qr/\b(?<= of )water\b/ => $ec_water || "cyan";
        recolor qr/\b(?<= of holy )water\b/ => $ec_water || "cyan";
        recolor qr/\b(?<= of unholy )water\b/ => $ec_water || "cyan";
        recolor qr/\b(?<= called )water\b/ => $ec_water || "cyan";
        recolor qr/\b(?<!un)holy(?= (?:\e\[[0-9;]*m)?water)\b/ => $ec_holywater || "bcyan";
        recolor qr/\bunholy(?= (?:\e\[[0-9;]*m)?water)\b/ => $ec_unholywater || "orange";
    }
# }}}
}
# }}}
# food conducts {{{
if (!$ece_food)
{
# vegan {{{
    if (!$ece_vegan)
    {
        recolor qr/\b(?:food |cram |[KC]-)rations?\b/ => $ec_vegan || "bgreen";
        recolor qr/\btins? (?:called|named|of) spinach/ => $ec_vegan || "bgreen";
        recolor qr/(?<!soft |hard |lled )\boranges?(?! dragon| gem| potion| spellbook| glass)\b/ => $ec_vegan || "bgreen";
        recolor qr/\bpears?\b/ => $ec_vegan || "bgreen";
        recolor qr/\b(?:gunyoki|lembas wafer|melon|carrot|pear|apple|banana|kelp frond|slime mold|brain)s?\b/ => $ec_vegan || "bgreen";
        recolor qr/\bsprigs? of wolfsbane\b/ => $ec_vegan || "bgreen";
        recolor qr/\beucalyptus lea(?:f|ves)\b/ => $ec_vegan || "bgreen";
        recolor qr/\bcloves? of garlic\b/ => $ec_vegan || "bgreen";
        recolor qr/\b(?:tin of )?(?:gelatinous cube|acid blob|quivering blob|lichen|shrieker|violet fungus|(?:blue|spotted|ochre) jelly|(?:brown|yellow|green) mold)(?: corpse)?\b/ => $ec_vegan || "bgreen";
    }
# }}}
# vegetarian {{{
    if (!$ece_vegetarian)
    {
        recolor qr/\b(?:egg|pancake|fortune cookie|candy bar|cream pie)s?\b/ => $ec_vegetarian || "green";
        recolor qr/\blumps? of royal jelly\b/ => $ec_vegetarian || "green";
        recolor qr/\b(?:tin of )?(?:brown pudding|gray ooze)(?: corpse)?\b/ => $ec_vegetarian || "green";
    }
# }}}
}
# }}}
# goodies {{{
if (!$ece_goodies)
{
    recolor qr/(?<!cursed )\bbag $called holding\b/ => $ec_boh || $ec_goody || "magenta" unless $ece_boh;
    recolor qr/(?<!cursed )\b(?:stone called )?luck(?:stone)?\b/ => $ec_luckstone || $ec_goody || "magenta" unless $ece_luckstone;
    recolor qr/\bwand $called wishing\b/ => $ec_wishing || $ec_goody || "magenta" unless $ece_wishing;
    recolor qr/\bmagic marker\b/ => $ec_marker || $ec_goody || "magenta" unless $ece_marker;
    recolor qr/\bmagic lamp\b/ => $ec_magiclamp || $ec_goody || "magenta" unless $ece_magiclamp;
    recolor qr/\blamp called magic\b/ => $ec_magiclamp || $ec_goody || "magenta" unless $ece_magiclamp;
    recolor qr/(?<!cursed )\bunicorn horn\b(?!\s+\[)/ => $ec_unihorn || $ec_goody || "magenta" unless $ece_unihorn;
    recolor qr/\btinning kit\b/ => $ec_tinkit || "magenta" unless $ece_tinkit;
    recolor qr/\bring $called regen(?:eration)?\b/ => $ec_regen || $ec_goody || "magenta" unless $ece_regen;
    recolor qr/\bring $called conflict\b/ => $ec_conflict || $ec_goody || "magenta" unless $ece_conflict;
    recolor qr/\bring $called (?:FA|free action)\b/ => $ec_freeaction || $ec_goody || "magenta" unless $ece_freeaction;
    recolor qr/\bring $called (?:TC|teleport control)\b/ => $ec_tc || $ec_goody || "magenta" unless $ece_tc;
    recolor qr/\bring $called lev(?:itation)?\b/ => $ec_lev || $ec_goody || "magenta" unless $ece_lev;
    recolor qr/\bamulet $called (?:LS|life ?saving)\b/ => $ec_ls || $ec_goody || "magenta" unless $ece_ls;
    recolor qr/\bamulet $called ref(?:lection)?\b/ => $ec_ref || $ec_goody || "magenta" unless $ece_ref;
    recolor qr/\bc(?:o|hi)ckatrice (?:corpse|egg)s?\b/ => $ec_trice || $ec_goody || "magenta" unless $ece_trice;
    recolor qr/\beggs? named cockatrice\b/ => $ec_trice || $ec_goody || "magenta" unless $ece_trice;
    recolor qr/\blizard corpses?\b/ => $ec_lizard || $ec_goody || "magenta" unless $ece_lizard;
    recolor qr/\bstethoscope\b/ => $ec_scope || $ec_goody || "magenta" unless $ece_scope;
    recolor qr/\bwraith corpses?\b/ => $ec_wraith || $ec_goody || "magenta" unless $ece_wraith;
}
# instruments {{{
if (!$ece_instrument)
{
    recolor qr/\b(?:(?:tooled|fire|frost)? horn)\b/ => $ec_instrument || $ec_goody || "magenta";
    recolor qr/\bhorn $called (?:tooled|fire|frost)\b/ => $ec_instrument || $ec_goody || "magenta";
    recolor qr/\b(?:magic|wooden) (?:harp|flute)\b/ => $ec_instrument || $ec_goody || "magenta";
    recolor qr/\b(?:harp|flute) $called (?:magic|wooden)\b/ => $ec_instrument || $ec_goody || "magenta";
    recolor qr/\bbugle\b/ => $ec_instrument || $ec_goody || "magenta";
}
# }}}
# }}}
# artifacts {{{
if (!$ece_artifact)
{
# unaligned {{{
    recolor qr/\b(?:Dragonbane|Fire Brand|Frost Brand|Ogresmasher|Trollsbane|Werebane)\b/ => $ec_uartifact || $ec_artifact || "bgreen";
# }}}
#lawful {{{
    recolor qr/\b(?:Demonbane|Excalibur|Grayswandir|Snickersnee|Sunsword)\b/ => $ec_lartifact || $ec_artifact || "bgreen";
    recolor qr/(?:[Tt]he )?\b(?:Orb of Detection|Sceptre of Might|Magic Mirror of Merlin|Mitre of Holiness|Tsurugi of Muramasa)\b/ => $ec_qlartifact || $ec_qartifact || $ec_lartifact || $ec_artifact || "bgreen";
# }}}
#neutral {{{
    recolor qr/\b(?:Cleaver|Giantslayer|Magicbane|Mjollnir|Vorpal Blade)\b/ => $ec_nartifact || $ec_artifact || "bgreen";
    recolor qr/(?:[Tt]he )?\b(?:Heart of Ahriman|Staff of Aesculapius|Eyes of the Overworld|Platinum Yendorian Express Card|Orb of Fate|Eye of the Aethiopica)\b/ => $ec_qnartifact || $ec_qartifact || $ec_nartifact || $ec_artifact || "bgreen";
# }}}
#chaotic {{{
    recolor qr/\b(?:Grimtooth|Orcrist|Sting|Stormbringer)\b/ => $ec_cartifact || $ec_artifact || "bgreen";
    recolor qr/(?:[Tt]he )?\b(?:Longbow of Diana|Master Key of Thievery)\b/ => $ec_qcartifact || $ec_qartifact || $ec_cartifact || $ec_artifact || "bgreen";
}
# }}}
#invocation items {{{
    recolor qr/(?:[Tt]he )?(?<!cursed )\b(?:Bell of Opening|silver bell|Candelabrum of Invocation|candelabrum|Book of the Dead|papyrus spellbook)\b/ => $ec_invocation || "bmagenta" unless $ece_invocation;
# }}}
#raison d'etre {{{
    recolor qr/(?:[Tt]he )?\bAmulet of Yendor(?= named\b)/ => $ec_yendor || "bmagenta" unless $ece_yendor;
# }}}
# }}}
# cursed goodies {{{
if (!$ece_goodies)
{
    recolor qr/\bcursed bag $called holding\b/ => $ec_cboh || "bred" unless $ece_cboh;
    recolor qr/\bcursed (?:stone called )?luck(?:stone)?\b/ => $ec_cluck || "bred" unless $ece_cluck;
    recolor qr/\bcursed unicorn horn\b(?!\s+\[)/ => $ec_chunihorn || "bred" unless $ece_chunihorn;
    recolor qr/\bcursed (?:Bell of Opening|silver bell|Candelabrum of Invocation|candelabrum|Book of the Dead|papyrus spellbook)\b/ => $ec_cinvocation || "bred" unless $ece_cinvocation;
}
# }}}
# bad stuff! {{{
recolor qr/\b(?:stone called )?(?<!your )load(?:stone)?\b/ => $ec_loadstone || "bred" unless $ece_loadstone;
# }}}
# watch out bag of holding {{{
if (!$ece_bohboom)
{
    recolor qr/\bbag $called tricks\b/ => $ec_bot || $ec_bohboom || "blue";
    recolor qr/\bwand $called [^\e]*?(?<!!)canc(?:ellation)?\b(?! named ${colors}e(?:mpty)?\b| (?:named .*?)?\($colors\d+$colors:${colors}(?:0|-1)$colors\))/ => $ec_canc || $ec_bohboom || "blue";
    recolor qr/\bwand $called (?:\w+ )?vanish(?:e[rs])?\b/ => $ec_vanish || $ec_canc || $ec_bohboom || "blue";
}
# }}}
# shinies {{{
recolor qr/\d+ (?:gold piece|[Zz]orkmid)s?\b/ => $ec_gold || "yellow" unless $ece_gold;
recolor qr/\bgems? $called valuable(?: \w+| yellowish brown)?\b/ => $ec_goodsoft || $ec_goodgem || "brown" unless $ece_gem;
recolor qr/\bgems? $called hard(?: \w+| yellowish brown)?\b/ => $ec_goodhard || $ec_goodgem || "yellow" unless $ece_gem;
# too tired to do this now {{{
#soft gems
#MENUCOLOR=" \([0-9]+\|an?\|gems? .*\) \(uncursed \|cursed \|blessed \)?\(dilithium\|opal\|garnet\|jasper\|agate\|jet\|obsidian\|jade\|citrine\|chrysoberyl\|amber\|amethyst\|fluorite\|turquoise\)\(e?s\)?\( stones?\| gems?\| crystals?\)?\( named .*\)?$"=brown
##hard gems
#MENUCOLOR=" \([0-9]+\|an?\|gems?.*\) \(uncursed \|cursed \|blessed \)?\(diamond\|rub\(y\|ies\)\|jacinth\|sapphire\|black opal\|emerald\|topaz\|aquamarine\)\(e?s\)?\( stones?\| gems?\)?\( named .*\)?$"=yellow
# }}}
# }}}
# interhack-specific stuff {{{
# charges (originally from doy) {{{
if (!$ece_charges_individual)
{
    recolor qr/(?<=\()0(?=:)/        => $ec_0_recharges || "cyan";  # 0 recharge
    recolor qr/(?<=:)(?:0|-1)(?=\))/ => $ec_0_charges || "red";   # no charges
    recolor qr/(?<=:)\d+(?=\))/      => $ec_recharges || "cyan";  # many charges
    recolor qr/(?<=\()\d+(?=:)/      => $ec_charges || "green"; # many recharges
}
elsif (!$ece_charges)
{
    recolor qr/\([\d-]+:\d+\)/         => $ec_charged || "cyan";
    recolor qr/\([\d-]+:0\)/           => $ec_0_charges || $ec_empty || "darkgray";
}
# }}}
# enchantment (originally from doy) {{{
if (!$ece_enchantment)
{
    recolor qr/\s\+0/               => $ec_plus0 || "brown";
    recolor qr/\s\+[1-3]/           => $ec_plus13 || $ec_plus || "green";
    recolor qr/\s\+[4-9]\d*/        => $ec_plus4 || $ec_plus || "bgreen";
    recolor qr/(?<!AC)\s\-[1-9]\d*/ => $ec_minus || "red";
}
# }}}
# empty wands and tools {{{
recolor qr/(?<=named )e(?:mpty)?\b/ => $ec_empty || "darkgray" unless $ece_empty;
# }}}
# item in use {{{
recolor qr/(?<=\()(?:\d candles, )?lit(?=\))/ => $ec_lit || "yellow" unless $ece_lit;
# equipment (originally by Stabwound) {{{
if (!$ece_wielded)
{
    recolor qr/ \(weapon in [^)]+\)/ => $ec_primary || $ec_weapon || $ec_eq || "brown";
    recolor qr/ \(wielded[^)]*\)/ => $ec_secondary || $ec_weapon || $ec_eq || "brown";
    recolor qr/ \(alternate weapon[^)]*\)/ => $ec_alternate || $ec_weapon || $ec_eq || "brown";
}

recolor qr/ \(in quiver\)/ => $ec_quiver || $ec_eq || "brown" unless $ece_quiver;

if (!$ece_worn)
{
    recolor qr/ \(being worn\)/ => $ec_worn || $ec_eq || "brown";
    recolor qr/ \(embedded in your skin\)/ => $ec_worn || $ec_eq || "brown";
    recolor qr/ \(on left [^)]+\)/ => $ec_worn || $ec_eq || "brown";
    recolor qr/ \(on right [^)]+\)/ => $ec_worn || $ec_eq || "brown";
    recolor qr/ \(in use\)/ => $ec_worn || $ec_eq || "brown";
}
# }}}
# }}}
# pretty useless items {{{
if (!$ece_useless)
{
# things explicitly named "crap" {{{
    recolor qr/\w+ called (?:crap|junk|worthless)\b/ => $ec_useless_crap || $ec_useless || "darkgray" unless $ece_useless_crap;
# }}}
# scrolls {{{
    recolor qr/scrolls? (?:called|of) (?:light|confuse monster|stinking cloud|punishment|fire|destroy armor|amnesia|create monster|food detection)\b/ => $ec_useless_scrolls || $ec_useless || "darkgray" unless $ece_useless_scrolls;
    recolor qr/scrolls? called (?:\w+\s+)+50(?!\/)/ => $ec_useless_scrolls || $ec_useless || "darkgray" unless $ece_useless_scrolls;
# }}}
# potions {{{
    recolor qr/(?!smoky )potions? (?:called|of) (?!smoky\b)(?:[\w-]+ )?(?:booze|sake|fruit juice|see invisible|sickness|deli|confusion|hallucination|restore ability|sleeping|blindness|invisibility|monster detection|obj(?:ect)? ?det(?:ection)?|(?:(?!1x)\d+x)?OD|levitation|polymorph|acid|oil|paralysis)\b/ => $ec_useless_potions || $ec_useless || "darkgray" unless $ece_useless_potions;

    # only 150 potion of note is gain energy, so we can color all 150 crap
    # three regex for '150' or 'NxOD' or both, for sanity reasons :)
    recolor qr/(?!smoky )potions? called (?!smoky\b)(?:[\w-]+\s+)+1?50(?!\/)(?: (?:(?!1x)\d+x)?OD)?/ => $ec_useless_potions || $ec_useless || "darkgray" unless $ece_useless_potions;
    recolor qr/(?!smoky )potions? called (?!smoky\b)(?:[\w-]+\s+)+ (?:(?!1x)\d+x)?OD/ => $ec_useless_potions || $ec_useless || "darkgray" unless $ece_useless_potions;
    recolor qr/(?!smoky )potions? called (?!smoky\b)(?:[\w-]+\s+)+1?50(?!\/)/ => $ec_useless_potions || $ec_useless || "darkgray" unless $ece_useless_potions;
# }}}
# rings {{{
    recolor qr/ring (?:of|called) (?:adornment|hunger|protection(?: from shape changers)?|stealth|sustain ability|warning|aggravate monster|\w+ resistance|gain \w+|increase \w+|see invisible|searching|polymorph(?: control)?)\b/ => $ec_useless_rings || $ec_useless || "darkgray" unless $ece_useless_rings;
    recolor qr/ring called (?:[\w-]+\s+)+1(?:0|5)0(?!\/)/ => $ec_useless_rings || $ec_useless || "darkgray" unless $ece_useless_rings; # only exception is =invis, which is borderline anyway
# }}}
# wands {{{
    recolor qr/wand (?:called|of) (?:light|nothing|locking|make invisible|opening|probing|secret door detection|(?:speed|slow)(?: monster)?|undead turning|create monster)\b/ => $ec_useless_wands || $ec_useless || "darkgray" unless $ece_useless_wands;
    recolor qr/wand called (?:\w+\s+)+(?:100|nomsg)(?!\/)/ => $ec_useless_wands || $ec_useless || "darkgray" unless $ece_useless_wands;
# }}}
# amulets {{{
    recolor qr/amulet (?:called|of) (?:versus poison|change|ESP|magical breathing|restful sleep|strangulation|unchanging)\b/ => $ec_useless_amulets || $ec_useless || "darkgray" unless $ece_useless_amulets;
    recolor qr/amulet versus poison\b/ => $ec_useless_amulets || $ec_useless || "darkgray" unless $ece_useless_amulets;
# }}}
}
# }}}
# unidentified magical armor {{{
if (!$ece_unid_armor)
{
    recolor qr/(?:mud|buckled|riding|snow|hiking|combat|jungle) boots/ => $ec_unid_boots || $ec_unid_armor || "green";
    recolor qr/piece of cloth|opera cloak|ornamental cope|tattered cape/ => $ec_unid_cloak || $ec_unid_armor || "green";
    recolor qr/(?:plumed|etched|crested|visored) helmet/ => $ec_unid_helmet || $ec_unid_armor || "green";
    recolor qr/(?:old|riding|padded|fencing) gloves/ => $ec_unid_gloves || $ec_unid_armor || "green";
}
# }}}
# goodies (other) {{{
if (!$ece_goodies_other)
{
# scrolls {{{
    recolor qr/scrolls? (?:called|of) (?:charging|genocide)\b/ => $ec_good_scrolls || $ec_goodies_other || "magenta" unless $ece_good_scrolls;
# }}}
# potions {{{
    recolor qr/potions? (?:called|of) (?:gain level|(?:full |extra )?healing)\b/ => $ec_good_potions || $ec_goodies_other || "magenta" unless $ece_good_potions;
# }}}
# wands {{{
    recolor qr/wand (?:called|of) (?:death|tele(?:portation)?)\b/ => $ec_good_wands || $ec_goodies_other || "magenta" unless $ece_good_wands;
# }}}
}
# }}}
# }}}

