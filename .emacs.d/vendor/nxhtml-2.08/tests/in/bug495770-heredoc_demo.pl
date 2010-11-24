#!/usr/bin/perl
# heredoc_demo.pl                   doom@kzsu.stanford.edu
#                                   December 10, 2009

use warnings;
use strict;
$|=1;
use Data::Dumper;

use File::Path     qw( mkpath );
use File::Basename qw( fileparse basename dirname );
use File::Copy     qw( copy move );
use Fatal          qw( open close mkpath copy move );
use Cwd            qw( cwd abs_path );

use Env qw(HOME);

our $VERSION = 0.01;
my  $prog    = basename($0);

use Getopt::Std;
my %opt = ();
getopts('d', \%opt);
my $DEBUG   = $opt{d} || 1;   # TODO set default to 0 when in production

my ($title, $incantation, $god);

my $skull=<<"END_SQL";
  SELECT id, god, incantation
  FROM spell, pantheon
  WHERE pantheon.id = spell.pantheon AND
        pantheon.name = 'lovecraft'
END_SQL

my $phfftp=<<"END_HTML";
<HTML><HEAD><TITLE>$title</TITLE></HEAD>
<BODY>
<H2>$title</H2>
<P>Speak not the dread words of $incantation
lest ye invoke the $god.</P>
</BODY></HTML>
END_HTML

print $skull, "\n";
print $phfftp, "\n";

__END__

=head1 NAME

heredoc_demo.pl - (( TODO insert brief description ))

=head1 SYNOPSIS

  heredoc_demo.pl -[options] [arguments]

  Options:
     -d          debug

=head1 OPTIONS

=over 8

=item B<-d>

Turn on debug messages.

=back

=head1 DESCRIPTION

B<heredoc_demo.pl> is a script which

(( TODO  insert explaination
   This is stub documentation created by template.el.  ))

=head1 AUTHOR

Joseph Brenner, E<lt>doom@kzsu.stanford.eduE<gt>

=head1 COPYRIGHT AND LICENSE

Copyright (C) 2009 by Joseph Brenner

This program is free software; you can redistribute it and/or modify it
under the terms of either: the GNU General Public License as published
by the Free Software Foundation; or the Artistic License.

See http://dev.perl.org/licenses/ for more information.

=head1 BUGS

None reported... yet.

=cut
