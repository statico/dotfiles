# this aims to strip the existing menucolors from a player on NAO
# should be safe
# by Eidolos

each_iteration
{
  s/\e\[[0-9;]*m( ?[a-zA-Z\$] - )/\e[0m$1/g
}

