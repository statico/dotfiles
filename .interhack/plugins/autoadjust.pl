# adds a new helper function autoadjust. when you pick up an item matching the
# regex it'll ask if you want to adjust to the specified letter (unless the
# item is already in that slot). it should ignore most things such as
# BUC/grease/damage/enchantment/name/charges etc
# by Eidolos

# NOTE: due to the way Perl parses this, you need to have either "sub
# autoadjust;" in your config before any autoadjust statements OR put parens
# around those statements. looking for a fix.

my %autoadjusts;

sub autoadjust
{
    my ($item, $adjust_to) = @_;
    my %adjust_bad;

    if (substr($adjust_to, 0, 1) ne '^')
    {
        $autoadjusts{$adjust_to}++;
    }
    else
    {
        %adjust_bad = map {$_ => 1} split //, substr($adjust_to, 1);
    }

    make_tab
             qr{ ^
                 (?-x:You have a little trouble lifting )?
                 (.)                                # inv slot
                 \ -\                               # literal
                 (an?|\d+)                          # count
                 [^.]*?                             # blessed, e.g.
                 \                                  # require space before item
                 $item                              # user regex
                 [^.]*?                             # rest of the item name
                 (?-x: named.*?)?                   # item name
                 (?: \  \( -? \d+ : -? \d+ \))?     # charges
                 (?-x: \(unpaid, \d+ zorkmids?\))?  # price
                 \.
               }x
          => sub
             {
                 if (substr($adjust_to, 0, 1) ne '^')
                 {
                     return $1 eq $adjust_to ? "" : "\e\e#adjust\n$1$adjust_to"
                 }
                 else
                 {
                     return "" if $1 ne substr($adjust_to, 1, 1);

                     # find an unallocated autoadjust slot
                     my @letters = ('a'..'z', 'A'..'Z');
                     while (local $_ = splice @letters, rand @letters, 1)
                     {
                         if (!exists($autoadjusts{$_}) && !exists($adjust_bad{$_}))
                         {
                             return "\e\e#adjust\n$1$_";
                         }
                     }

                     # ok so we have autoadjusts for all 52 slots, this is
                     # getting silly, so we'll just pick "a" or "b"
                     if ($adjust_bad{a})
                     {
                         return "\e\e#adjust\n$1b";
                     }
                     else
                     {
                         return "\e\e#adjust\n$1a";
                     }
                 }
             };
}

our $aa_key         = qr/key|lock pick|credit card|osaku/;
our $aa_unihorn     = qr/unicorn horn/;
our $aa_athame      = qr/Magicbane|athame/;
our $aa_stethoscope = qr/stethoscope/;
our $aa_pickaxe     = qr/pick-axe|mattock/;
our $aa_bag         = qr/bag(?! (?:called|of) tricks)|sack|holding|oilskin/;
our $aa_Amulet      = qr/(?<!imitation of the )Amulet of Yendor/;
our $aa_blindfold   = qr/towel|blindfold/;
our $aa_lizard      = qr/lizard corpse/;
our $aa_conflict    = qr/ring (?:of|called) conflict|Sceptre of Might/;
our $aa_whistle     = qr/magic whistle|whistle called magic/;
our $aa_lamp        = qr/lamp|lantern|lamp called (?:magic|oil)/;
our $aa_luckstone   = qr/luck(?:stone)?|Heart of Ahriman/;
our $aa_levitation  = qr/lev(?:itation)?/;
our $aa_instrument  = qr/(?<!unicorn )horn|(?:wooden |magic )?(?:flute|harp)|bugle/;
our $aa_trice       = qr/c(?:o|hi)ckatrice corpse/;

# samples (see *-config for more):
# sub autoadjust;
# autoadjust $aa_key => "k";
# autoadjust $aa_unihorn => "h";
# autoadjust $aa_athame => "E";
# autoadjust qr/potions? of healing/ => "H";
# autoadjust $aa_trice => "^ye"; # try to keep cockatrices off "y" and "e"
# autoadjust qr/\bpotions?\b[^.]*?/ => "^q";
# autoadjust qr/\bwand\b[^.]*?/ => "^z";

