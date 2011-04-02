# adds an extended command #eval for evaluating Perl code
# I don't think this has any security holes, but one can never be too sure
# this requires that you set $load_eval to 1 before including *, just so you
# know what you're dealing with
# set $no_evalcontext to a true value if you don't want to install
# Devel::EvalContext

# this can be useful because it's so damn flexible.. for example, re-remapping
# keys: #eval $keymap{"\ce"} = "EE  Elbereth\n"
# set a reminder: #eval press_tab sub{$vt->row_text(24)=~/^Dlvl:9\b/}=>"enchant
# stuff plz"

# note: if the "#eval <code>: unknown extended command" output exceeds one line,
# it won't work. One solution is to put <code> into a file and use
# #eval do 'eval.pl'

# by Eidolos

if ($load_eval)
{
    # for persistency across #evals
    require Devel::EvalContext unless $no_evalcontext;

    extended_command "#eval"
                  => sub
                  {
                      my ($command, $args) = @_;

                      # need to make sure $is_playing is entirely accurate
                      return "#eval disabled until it's [more] secure";

                      return "Syntax: #eval CODE" if !defined($args) || $args eq '';
                      my $ret = eval $args;
                      return "undef" if !defined($ret);
                      return $ret;
                  }
}

