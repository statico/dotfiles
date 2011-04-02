# dear vi keys user, ever accidentally walk northwest when asked if you really
# want to attack something? yeah
# by Eidolos

each_match qr/^Really attack .*\? \[yn\] \(n\)\s*$/ => sub
{
    my $confirm_swing;
    $confirm_swing = sub
    {
        my $dir = shift;
        my $swing = force_tab_yn("Press tab to really attack, any other key to cancel!");
        return $dir if $swing;

        # make sure it's set again
        $keyonce{y} = $keyonce{Y} = $confirm_swing;
        return '';
    };
    $keyonce{y} = $keyonce{Y} = $confirm_swing;
}

