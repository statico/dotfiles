# this plugin lets you view maps of other levels
# the syntax is #view DLVL
# currently somewhat limited because there can only be one map for dlvl 5 even
# though dlvl 5 can describe a main branch level, a mines level, and a Soko
# level
# by Eidolos

include "stats";

our %map;

$vt->callback_set("CLEAR", sub
{
    my ($cbvt, $cbtype) = @_;
    return unless $cbtype eq "CLEAR";
    return unless $cbvt->row_plaintext(24) =~ /^(?:Dlvl:\d+|Home \d+|End Game|Astral Plane) /;

    $mapdlvl = $1 if $dlvl =~ /Dlvl:(\d+)/;
    $mapdlvl = "q$1" if $dlvl =~ /Home (\d+)/;
    $map{$mapdlvl} = [map {$cbvt->row_plaintext($_)} 2..22];
});

extended_command "#view"
              => sub
              {
                  my ($cmd, $args) = @_;

                  $args = '' if !defined($args);
                  $args =~ s/(\d+)//;
                  return "Syntax: #view DLVL [or #view FROM-TO]" if !defined($1);

                  my $from = $1;
                  my $to = $from;
                  $to = $1 if $args =~ s/(\d+)//;

                  for my $level ($from..$to)
                  {
                      if (!exists($map{$level}))
                      {
                          request_redraw() if $level > $from;
                          return "I don't have a map for $level.";
                      }

                      print_ih_ttyrec("\e[s\e[1;30m\e[2H");
                      print "\e[s\e[1;30m\e[2H";
                      for (@{$map{$level}})
                      {
                          local $_ = substr($_, 0, 79) . "\n";
                          print_ih_ttyrec($_);
                          print;
                      }

                      local $_ = "\e[m\e[HDrawing dlvl $level. Press a key to redraw the screen.--More--";
                      print_ih_ttyrec($_);
                      print;
                      ReadKey defined $ttyrec ? 5 : 0;
                  }
                  request_redraw();
                  "If you can read this, you have pretty quick eyes!"
              };

