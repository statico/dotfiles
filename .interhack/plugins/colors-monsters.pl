# color monster names in messages and menus and things
# by toft and Eidolos

# shorcut regexes {{{
# so we can write e.g. "el$ves" instead of "el(?:f|ves)"
my $ves = qr/f|ves/;
my $the = qr/(?:[Tt]he )?/;
my $us = qr/us|i/;
my $es = qr/(?:es)?/;
my $ies = qr/y|ies/;
my $ex = qr/ex|ices/;
my $esses = qr/esses?|s?/;
# }}}

# chromatic stuff {{{
my @dim_colors = qw/red brown green cyan magenta blue/;
my @bold_colors = map {$colormap{$_}} map {"b$_"} @dim_colors;
@dim_colors = map {$colormap{$_}} @dim_colors;

my @colors = @dim_colors, @bold_colors;

sub random_color { $colors[     rand @colors     ] }
sub random_dim   { $dim_colors[ rand @dim_colors ] }
sub random_bold  { $bold_colors[rand @bold_colors] }
# }}}

# deities {{{
if ($chromatic_gods)
{
  each_iteration { s/Marduk|Moloch|Quetzalcoatl|Camaxtli|Huhetotl|Mitra|Crom|Set(?! what opt)|Anu|Ishtar|Anshar|Athena|Hermes|Poseidon|Lugh|Brigit|Manannan Mac Lir|Shan Lai Ching|Chih Sung-tzu|Huan Ti|Mercury|Venus|Mars|Issek|Mog|Kos|Amaterasu Omikami|Raijin|Susanowo|Blind Io|The Lady|Offler|Tyr|Odin|Loki|Ptah|Thoth|Anhur/(join '', map {random_bold()."$_"} split '', $&)."\e[0m"/eg }
}
else
{
  recolor qr/Marduk|Moloch|Quetzalcoatl|Camaxtli|Huhetotl|Mitra|Crom|Set(?! what opt)|Anu|Ishtar|Anshar|Athena|Hermes|Poseidon|Lugh|Brigit|Manannan Mac Lir|Shan Lai Ching|Chih Sung-tzu|Huan Ti|Mercury|Venus|Mars|Issek|Mog|Kos|Amaterasu Omikami|Raijin|Susanowo|Blind Io|The Lady|Offler|Tyr|Odin|Loki|Ptah|Thoth|Anhur/ => \&random_bold;
}
# }}}

# team a {{{
recolor qr/giant ants?/ => "brown";
recolor qr/killer bees?/ => "yellow";
recolor qr/soldier ants?/ => "blue";
recolor qr/fire ants?/ => "red";
recolor qr/giant beetles?/ => "nhblack";
recolor qr/queen bees?/ => "purple";
# }}}
# blobs b {{{
recolor qr/acid blobs?/ => "green";
recolor qr/quivering blobs?/ => "white";
recolor qr/gelatinous cubes?/ => "cyan";
# }}}
# cockatrices c {{{
recolor qr/chickatrices?(?! corpse)/ => "brown";
recolor qr/cockatrices?(?! corpse| egg)/ => "yellow";
recolor qr/pyrolisks?/ => "red";
# }}}
# canines d {{{
recolor qr/(?<!were)jackals?|coyotes?/ => "brown";
recolor qr/(?<!ter |were)wol$ves(?!sbane)|wargs?/ => "brown";
recolor qr/fox$es/ => "red";
recolor qr/dingos?/ => "yellow";
recolor qr/(?:little|large)? dogs?/ => "bwhite";
recolor qr/winter wolf cubs?|winter wol$ves/ => "cyan";
recolor qr/hell hound(?: pup)?s?/ => "red";
# }}}
# eyes and spheres e {{{
recolor qr/gas spores?/ => "gray";
recolor qr/floating eyes?/ => "bcyan";
recolor qr/freezing spheres?/ => "bwhite";
recolor qr/flaming spheres?/ => "red";
recolor qr/shocking spheres?/ => "bblue";
# }}}
# felines f {{{
recolor qr/kittens?|(?:house|large )cats?/ => "bwhite";
recolor qr/jaguars?/ => "brown";
recolor qr/lynx(?:es)?/ => "cyan";
recolor qr/panthers?/ => "nhblack";
recolor qr/tigers?(?! eye)/ => "yellow";
# }}}
# gremlins and gargoyles g {{{
recolor qr/gremlins?/ => "green";
recolor qr/(?<!winged )gargoyles?/ => "brown";
recolor qr/winged gargoyles?/ => "purple";
# }}}
# humanoids h {{{
recolor qr/hobbits?/ => "green";
recolor qr/dwar$ves(?! lord| king| zombie| mumm)/ => "red";
recolor qr/bugbears?/ => "brown";
recolor qr/dwarf lords?/ => "blue";
recolor qr/dwarf kings?/ => "purple";
recolor qr/(?:master )?mind flayers?/ => "purple";
# }}}
# minor demons i {{{
recolor qr/manes|imps?\b/ => "red";
recolor qr/homuncul$us/ => "green";
recolor qr/lemures?/ => "brown";
recolor qr/quasits?/ => "blue";
recolor qr/tengus?/ => "cyan";
# }}}
# jellies j {{{
recolor qr/blue jell$ies/ => "blue";
recolor qr/spotted jell$ies/ => "green";
recolor qr/ochre jell$ies/ => "brown";
# }}}
# kobolds k {{{
recolor qr/(?<!large )kobolds?(?! lord| shaman| zombie| mumm)/ => "brown";
recolor qr/large kobolds?/ => "red";
recolor qr/kobold lords?/ => "purple";
recolor qr/kobold shamans?/ => "bblue";
# }}}
# leprechauns l {{{
recolor qr/leprechauns?/ => "green";
# }}}
# mimics m {{{
recolor qr/small mimics?/ => "brown";
recolor qr/large mimics?/ => "red";
recolor qr/giant mimics?/ => "purple";
# }}}
# nymphs n {{{
recolor qr/wood nymphs?/ => "green";
recolor qr/water nymphs?/ => "blue";
recolor qr/mountain nymphs?/ => "brown";
# }}}
# orcs o {{{
recolor qr/(?<!hob)goblins?/ => "gray";
recolor qr/hobgoblins?/ => "brown";
recolor qr/(?<!hill |rdor )orcs?(?! zombie| mumm| shaman|-captain)\b/ => "red";
recolor qr/hill orcs?/ => "yellow";
recolor qr/Mordor orcs?/ => "blue";
recolor qr/Uruk-hai/ => "nhblack";
recolor qr/orc shamans?/ => "bblue";
recolor qr/orc-captains?/ => "purple";
# }}}
# piercers p {{{
recolor qr/rock piercers?/ => "gray";
recolor qr/iron piercers?/ => "cyan";
recolor qr/glass piercers?/ => "bwhite";
# }}}
# quadrupeds q {{{
recolor qr/rothes?/ => "brown";
recolor qr/mumaks?|titanotheres?|baluchitheri(?:um|a)/ => "gray";
recolor qr/leocrottas?/ => "red";
recolor qr/wumpus$es/ => "cyan";
recolor qr/mastodons?/ => "nhblack";
# }}}
# rodents r {{{
recolor qr/(?:sewer |rabid |giant |were)rats?/ => "brown";
recolor qr/rock moles?/ => "gray";
recolor qr/woodchucks?/ => "bpurple";
# }}}
# arachnids or centipedes s {{{
recolor qr/cave spiders?/ => "gray";
recolor qr/centipedes?/ => "yellow";
recolor qr/giant spiders?/ => "purple";
recolor qr/scorpions?/ => "red";
# }}}
# trappers or lurkers above t {{{
recolor qr/lurkers? above/ => "gray";
recolor qr/trappers?/ => "green";
# }}}
# horses and unicorns u {{{
recolor qr/white unicorns?/ => "white";
recolor qr/gray unicorns?/ => "gray";
recolor qr/black unicorns?/ => "nhblack";
recolor qr/pon$ies|(?:war)?horses?\b/ => "brown";
# }}}
# clouds and vortices v {{{
recolor qr/fog clouds?/ => "gray";
recolor qr/dust vort$ex/ => "brown";
recolor qr/ice vort$ex/ => "cyan";
recolor qr/energy vort$ex/ => "bblue";
recolor qr/steam vort$ex/ => "blue";
recolor qr/fire vort$ex/ => "yellow";
# }}}
# worms w {{{
recolor qr/(?:baby )?long worms?/ => "brown";
recolor qr/(?:baby )?purple worms?/ => "purple";
# }}}
# fantastical insects x {{{
recolor qr/grid bugs?/ => "purple";
recolor qr/xans?/ => "red";
# }}}
# lights y {{{
recolor qr/yellow lights?/ => "yellow";
recolor qr/black lights?/ => "nhblack";
# }}}
# zruties z {{{
recolor qr/zrut$ies/ => "brown";
# }}}

# angelic beings A {{{
recolor qr/couatls?/ => "green";
recolor qr/Aleax$es/ => "yellow";
recolor qr/Angels?/ => "bwhite";
recolor qr/ki-rin/ => "yellow";
recolor qr/Archons?/ => "purple";
# }}}
# bats and birds B {{{
# the following shouldnt match giant, vampire, and combat boots
# but no-variable length negative lookaround so it might screw up. XXX
each_iteration { s/giant bat(s?)/\e[31mgiant b\e[31mat$1\e[0m/g }
recolor qr/vampire bats?/ => "nhblack";
each_iteration { s/vampire bat(s?)/vampire b\e[C\e[Dat$1/g }
recolor qr/(?<!com)bats?\b/ => "brown";
recolor qr/ravens?/ => "nhblack";
# }}}
# centaurs C {{{
recolor qr/plains centaurs?/ => "brown";
recolor qr/forest centaurs?/ => "green";
recolor qr/mountain centaurs?/ => "cyan";
# }}}
# dragons D {{{
recolor qr/(?:baby )?gray dragons?(?! scale)/ => "gray";
recolor qr/(?:baby )?silver dragons?(?! scale)/ => "bcyan";
recolor qr/(?:baby )?red dragons?(?! scale)/ => "red";
recolor qr/(?:baby )?white dragons?(?! scale)/ => "bwhite";
recolor qr/(?:baby )?orange dragons?(?! scale)/ => "orange";
recolor qr/(?:baby )?black dragons?(?! scale)/ => "nhblack";
recolor qr/(?:baby )?blue dragons?(?! scale)/ => "blue";
recolor qr/(?:baby )?green dragons?(?! scale)/ => "green";
recolor qr/(?:baby )?yellow dragons?(?! scale)/ => "yellow";
# }}}
# elementals and stalkers E {{{
recolor qr/stalkers?/ => "bwhite";
recolor qr/air elementals?/ => "cyan";
recolor qr/fire elementals?/ => "yellow";
recolor qr/earth elementals?/ => "brown";
recolor qr/water elementals?/ => "blue";
# }}}
# fungi and molds F {{{
recolor qr/lichens?/ => "bgreen";
recolor qr/brown molds?/ => "brown";
recolor qr/yellow molds?/ => "yellow";
recolor qr/green molds?/ => "green";
recolor qr/red molds?/ => "red";
recolor qr/shriekers?|violet fung$us/ => "purple";
# }}}
# gnomes G {{{
recolor qr/gnomes?(?! lord| king| zombie| mumm)/ => "brown";
recolor qr/gnome lords?/ => "blue";
recolor qr/gnomish wizards?/ => "bblue";
recolor qr/gnome kings?/ => "purple";
# }}}
# large humanoids H {{{
recolor qr/(?<!tone |hill |fire |rost |torm )giants?(?! ant| beetle| mimic| eel| spider| rat| zombie| mumm| b(?:\e\[31m)?at)/ => "red";
recolor qr/stone giants?/ => "gray";
recolor qr/hill giants?/ => "cyan";
recolor qr/fire giants?/ => "yellow";
recolor qr/frost giants?/ => "bwhite";
recolor qr/storm giants?/ => "blue";
recolor qr/ettins?\b(?! zombie| mumm)|minotaurs?/ => "brown";
recolor qr/titan(?!otheres?)s?/ => "purple";
# }}}
# jabberwocks J {{{
recolor qr/jabberwocks?/ => "bred";
# }}}
# Keystone Kops K {{{
recolor qr/Keystone Kops?|Kop Sergeants?/ => "blue";
recolor qr/Kop Lieutenants?/ => "cyan";
recolor qr/Kop Kaptains?/ => "purple";
# }}}
# liches L {{{
recolor qr/(?:demi|master |arch-)?lich$es(?!en)/
     => sub
        {
            substr($&, 0, 1) eq 'd' ? $colormap{red}
          : substr($&, 0, 1) eq 'l' ? $colormap{brown}
          :                           $colormap{purple};
        };
# }}}
# mummies M {{{
recolor qr/kobold mumm$ies/ => "brown";
recolor qr/(?:orc|human) mumm$ies/ => "white";
recolor qr/(?:gnome|dwarf) mumm$ies/ => "bwhite";
recolor qr/elf mumm$ies/ => "green";
recolor qr/ettin mumm$ies/ => "blue";
recolor qr/giant mumm$ies/ => "cyan";
# }}}
# nagas N {{{
recolor qr/red nagas?(?: hatchlings?)?/ => "red";
recolor qr/black nagas?(?: hatchlings?)?/ => "nhblack";
recolor qr/golden nagas?(?: hatchlings?)?/ => "yellow";
recolor qr/guardian nagas?(?: hatchlings?)?/ => "green";
# }}}
# ogres O {{{
recolor qr/(?<!pr)ogres?\b(?! lord| king)/ => "brown";
recolor qr/ogre lords?/ => "red";
recolor qr/ogre kings?/ => "purple";
# }}}
# puddings and other amoeboids P {{{
recolor qr/gray oozes?/ => "gray";
recolor qr/brown puddings?/ => "brown";
recolor qr/black puddings?/ => "nhblack";
recolor qr/green slimes?/ => "green";
# }}}
# quantum mechanics Q {{{
recolor qr/quantum mechanics?/ => "cyan";
# }}}
# rust monsters and disenchanters R {{{
recolor qr/rust monsters?/ => "brown";
recolor qr/disenchanters?/ => "blue";
# }}}
# snakes S {{{
recolor qr/garter snakes?/ => "green";
recolor qr/(?<!garter )snakes?/ => "brown";
recolor qr/water moccasins?/ => "red";
recolor qr/pit vipers?|cobras?/ => "blue";
recolor qr/pythons?/ => "purple";
# }}}
# trolls T {{{
recolor qr/(?<!ice |ock |ter )trolls?\b/ => "brown";
recolor qr/ice trolls?/ => "bwhite";
recolor qr/rock trolls?/ => "cyan";
recolor qr/water trolls?/ => "blue";
recolor qr/Olog-hai/ => "purple";
# }}}
# umber hulks U {{{
recolor qr/umber hulks?/ => "brown";
# }}}
# vampires V {{{
recolor qr/vampires?(?! lord| b(?:\e\[C\e\[D)?at)/ => "red";
recolor qr/vampire lords?/ => "blue";
recolor qr/Vlad the Impaler/ => "purple";
# }}}
# wraiths W {{{
recolor qr/barrow wights?/ => "gray";
recolor qr/wraiths?/ => "nhblack";
recolor qr/Nazguls?/ => "purple"; # yes it's "9 Nazgul" but NetHack sucks!
# }}}
# xorns X {{{
recolor qr/xorns?/ => "brown";
# }}}
# apelike creatures Y {{{
recolor qr/monkeys?|sasquatch$es/ => "gray";
recolor qr/carnivorous apes?/ => "nhblack";
each_iteration { s/carnivorous ape(s?)/carnivorous a\e[C\e[Dpe$1/g };
recolor qr/(?<![s ][ch])apes?\b|owlbears?/ => "brown";
recolor qr/yetis?/ => "bwhite";
# }}}
# zombies Z {{{
recolor qr/(?:kobold|gnome) zombies?/ => "brown";
recolor qr/orc zombies?/ => "gray";
recolor qr/dwarf zombies?/ => "red";
recolor qr/elf zombies?/ => "green";
recolor qr/human zombies?/ => "bwhite";
recolor qr/ettin zombies?/ => "blue";
recolor qr/giant zombies?/ => "cyan";
recolor qr/ghouls?/ => "nhblack";
recolor qr/skeletons?(?! key)/ => "bwhite";

# }}}

# golems ' {{{
recolor qr/(?:straw|gold) golems?/ => "yellow";
recolor qr/paper golems?/ => "bwhite";
recolor qr/(?:rope|leather|wood|clay) golems?/ => "brown";
recolor qr/flesh golems?/ => "red";
recolor qr/stone golems?/ => "gray";
recolor qr/(?:iron|glass) golems?/ => "cyan";
# }}}
# humans and elves @ {{{
recolor qr/humans?(?!oid| zombie| mumm)/ => "bwhite";
recolor qr/wererats?/ => "brown";
recolor qr/werejackals?/ => "red";
recolor qr/werewol$ves/ => "bred";
# should stop yourself among other things
recolor qr/(?<![dny]-|[r ]s|ts)el$ves(?!-lord| zombie| mumm|-knowledgeable)/ => "bwhite";
recolor qr/Woodland-el$ves/ => "green";
recolor qr/Green-el$ves/ => "bgreen";
recolor qr/Grey-el$ves/ => "gray";
recolor qr/elf-lords?/ => "bblue";
recolor qr/Elvenkings?/ => "purple";
recolor qr/doppelgangers?/ => "bwhite";
recolor qr/nurses?/ => "bwhite";
recolor qr/shopkeepers?/ => "bwhite";
recolor qr/guards?\b/ => "blue";
recolor qr/prisoners?/ => "bwhite";
recolor qr/${the}Oracle/ => "bblue";
recolor qr/(?<!high )priest$esses/ => "bwhite";
recolor qr/high priest$esses/ => "bred"; # XXX: changed from bwhite
recolor qr/soldiers?(?! ant)/ => "gray";
recolor qr/sergeants?/ => "red";
recolor qr/lieutenants?/ => "green";
recolor qr/(?<!orc-|tch )captains?/ => "blue";
recolor qr/watchm[ae]n/ => "gray";
recolor qr/watch captains?/ => "green";
recolor qr/Medusa/ => "bgreen";
recolor qr/${the}Wizard of Yendor/ => "purple";
recolor qr/Croesus/ => "purple";
# }}}
# ghosts and shades X {{{
recolor qr/ghosts?/ => "white";
recolor qr/shades?/ => "blue";
# }}}
# demons & {{{
recolor qr/Death|Pestilence|Famine/ => "purple";
recolor qr/Orcus|Baalzebub|Asmodeus/ => "purple";
recolor qr/Juiblex/ => "bgreen";
recolor qr/Yeenoghu|Geryon|Dispater|Demogorgon/ => "purple";

recolor qr/djinni?/ => "yellow";
recolor qr/balrogs?|pit fiends?|nalfeshnees?/ => "red";
recolor qr/horned devils?/ => "brown";
recolor qr/barbed devils?|mariliths?|vrocks?|hezrous?/ => "red";
recolor qr/(?:suc|in)cub$us|sandestins?|bone devils?/ => "gray";
recolor qr/ice devils?/ => "white";
recolor qr/mail daemons?/ => "bblue";
recolor qr/water demons?/ => "blue";
# }}}
# sea monsters ; {{{
recolor qr/jellyfish/ => "blue";
recolor qr/piranhas?|krakens?/ => "red";
recolor qr/sharks?/ => "white";
recolor qr/giant eels?/ => "cyan";
recolor qr/electric eels?/ => "bblue";
# }}}
# lizards : {{{
recolor qr/newts?/ => "yellow";
recolor qr/geckos?|lizards?(?! corpse)/ => "green";
recolor qr/chameleons?|(?:baby )?crocodiles?|iguanas?/ => "brown";
recolor qr/salamanders?/ => "bred";
# }}}

# player monsters {{{
recolor qr/archeologists?|barbarians?|cave(?:wo)?m[ae]n|healers?|knights?|monks?\b|(?<!high )priest$esses(?! of )|rangers?|rogues?|samurai|tourists?|valkyries?|(?<!ish|old) wizards?(?! lock)/ => "white";
# }}}

# quest nemeses {{{
if ($chromatic_nemeses)
{
    recolor qr/${the}Minion of Huhetotl/ => "red";
    recolor qr/Thoth Amon/               => "purple";
    recolor qr/${the}Chromatic Dragon/   => "purple";
    recolor qr/${the}Cyclops/            => "gray";
    recolor qr/Ixoth/                    => "red";
    recolor qr/Master Kaen/              => "purple";
    recolor qr/Nalzok/                   => "red";
    recolor qr/Scorpius/                 => "purple";
    recolor qr/${the}Master Assassin/    => "purple";
    recolor qr/Ashikaga Takauji/         => "purple";
    recolor qr/Lord Surtur/              => "purple";
    recolor qr/${the}Dark One/           => "nhblack";
}
else
{
    recolor qr/${the}Minion of Huhetotl/ => \&random_color;
    recolor qr/Thoth Amon/               => \&random_color;
    recolor qr/${the}Chromatic Dragon/   => \&random_color;
    recolor qr/${the}Cyclops/            => \&random_color;
    recolor qr/Ixoth/                    => \&random_color;
    recolor qr/Master Kaen/              => \&random_color;
    recolor qr/Nalzok/                   => \&random_color;
    recolor qr/Scorpius/                 => \&random_color;
    recolor qr/${the}Master Assassin/    => \&random_color;
    recolor qr/Ashikaga Takauji/         => \&random_color;
    recolor qr/Lord Surtur/              => \&random_color;
    recolor qr/${the}Dark One/           => \&random_color;
}
# }}}
# quest leaders {{{
recolor qr/Lord Carnarvon/          => "purple";
recolor qr/Pelias/                  => "purple";
recolor qr/Shaman Karnov/           => "purple";
recolor qr/Hippocrates/             => "purple";
recolor qr/King Arthur/             => "purple";
recolor qr/${the}Grand Master/      => "nhblack";
recolor qr/${the}Arch Priest/       => "white";
recolor qr/Orion/                   => "purple";
recolor qr/${the}Master of Thieves/ => "purple";
recolor qr/Lord Sato/               => "purple";
recolor qr/Twoflower/               => "bwhite";
recolor qr/${the}Norn/              => "purple";
recolor qr/Neferet the Green/       => "green";
# }}}
# quest guardians {{{
recolor qr/students?|chieftains?|neanderthals?|attendants?|pages?|abbots?|acolytes?|hunters?|thugs?|ninjas?|roshi|guides?|warriors?|apprentices?/ => "white";
# }}}
