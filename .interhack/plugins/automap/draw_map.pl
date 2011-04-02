#!/usr/bin/env perl
use strict;
use warnings;

my ($y, $x) = (1, 1);
my $set = 0;
my $file = shift or die "Need a filename";
($set, $y) = (1, shift) if @ARGV;
($set, $x) = (1, shift) if @ARGV;
push @ARGV, $file;

print "\e[$y;${x}H\e[1;30m";

while (<>)
{
  chomp;

  if ($. == 1 && /^\s*(\d+)\s*(\d+)\s*$/)
  {
    print "\e[$1;$2H" unless $set;
    next;
  }

  my $length = length;
  s/([. +S]+)/"\e[".length($1).'C'/eg;
  print "$_\e[B\e[${length}D";
}
print "\e[0m";

