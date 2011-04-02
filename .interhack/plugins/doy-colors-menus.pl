# based on Eidocolors

# spell menu {{{
recolor qr/[a-zA-Z] - [a-z ]+ \d\s+\w+\s+0%/ => "white";
recolor qr/[a-zA-Z] - [a-z ]+ \d\s+\w+\s+[1-9]%/ => "cyan";
recolor qr/[a-zA-Z] - [a-z ]+ \d\s+\w+\s+[12][0-9]%/ => "cyan";
recolor qr/[a-zA-Z] - [a-z ]+ \d\s+\w+\s+[3-6][0-9]%/ => "yellow";
recolor qr/[a-zA-Z] - [a-z ]+ \d\s+\w+\s+7[0-5]%/ => "yellow";
recolor qr/[a-zA-Z] - [a-z ]+ \d\s+\w+\s+7[6-9]%/ => "bred";
recolor qr/[a-zA-Z] - [a-z ]+ \d\s+\w+\s+8[0-9]%/ => "bred";
recolor qr/[a-zA-Z] - [a-z ]+ \d\s+\w+\s+9[0-9]%/ => "red";
recolor qr/[a-zA-Z] - [a-z ]+ \d\s+\w+\s+100%/ => "red";
recolor qr/[a-zA-Z] - [a-z ]+ \d\*.*[0-9]+%/ => "magenta";
# }}}

# enhance menu {{{
recolor qr/(?<![a-z] - )[a-z -]+\[(?:Unskilled|Basic|Skilled|Expert|Master|Grand Master)\]/ => "darkgray";
recolor qr/[a-z] - [a-z0-9\e\[; -]+?\[(?:Unskilled|Basic|Skilled|Expert|Master|Grand Master)\]/ => "bgreen";
recolor qr/\*[a-z0-9\e\[; -]+?\[(?:Unskilled|Basic|Skilled|Expert|Master|Grand Master)\]/ => "green";
recolor qr/\#[a-z0-9\e\[; -]+?\[(?:Unskilled|Basic|Skilled|Expert|Master|Grand Master)\]/ => "red";
recolor qr/\[Unskilled\]/ => "white";
recolor qr/\[Basic\]/ => "cyan";
recolor qr/\[Skilled\]/ => "yellow";
recolor qr/\[Expert\]/ => "bred";
recolor qr/\[Master\]/ => "red";
recolor qr/\[Grand Master\]/ => "magenta";
# }}}
