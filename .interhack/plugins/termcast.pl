# automatically send our display over to termcast
# you need to have the following setup for this to work:

# $termcast_name = 'whatever';
# $termcast_pass = 'whatever';
# $write_interhack_ttyrec = 1;

if ($termcast_name) {
    my $termcast = IO::Socket::INET->new(PeerAddr => '213.184.131.118',
                                         PeerPort => 31337,
                                         Proto => 'tcp');
    die "Unable to connect to termcast: $@" if $@;
    print $termcast "hello $termcast_name $termcast_pass\n";

    sub print_to_termcast {
        print $termcast $_ for @_;
    }

    push @after_ih_ttyrec, \&print_to_termcast;
}

