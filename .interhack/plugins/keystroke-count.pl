# adds extended command #keys to display keystrokes so far
# by Eidolos

extended_command "#keys"
              => sub { "$keystrokes keystrokes thus far.\e[K" }

