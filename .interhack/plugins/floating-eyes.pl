# make floating eyes bright cyan
# by Eidolos

each_iteration
{
    s{
        (?<!\e\[1m)   # avoid coloring shocking spheres
        \e\[          # escape code initiation
        (?:0;)? 34m   # look for dark blue of floating eyes
        ((?:\x0f)? e) # look for e with or without DEC sequence
        (?! - )       # avoid false positive with menucolors
    }
    {\e[1;36m$1}xg;
}

