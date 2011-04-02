# add a #grep! :D uses nhgrep
# by Eidolos

extended_command "#grep" =>
sub
{
    my ($command, $args) = @_;
    my $ttyrec = $normal_ttyrec_name ? $normal_ttyrec_name
            : $interhack_ttyrec_name ? $interhack_ttyrec_name
            : return "You need to be recording a ttyrec.";
    $args =~ s/'/'\\''/g;
    my $output = `perl lib/nhgrep --file --number '$args' '$ttyrec'`;
    return "No hits." if $output eq '';
    pline($output);
}

