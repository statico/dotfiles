# provides a shortcut for donating xl * 400 gold to priests
# if you want clairvoyance, just divide by 2 >_>
# by Eidolos and toft (idea by toft)

extended_command "#donate"
      => sub
      {
          "clairvoyance: \$" . $xlvl * 200 . ", protection: \$" . $xlvl * 400 . "\n"
      };
make_tab qr/^How much will you offer\?/
      => sub { $xlvl * 400 . "\n" };
