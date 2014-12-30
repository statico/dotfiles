require './lib/CoffeeTags'
require 'yaml'

out = Coffeetags::Utils.option_parser ARGV
if out
  output, include_vars, files  = out
  Coffeetags::Utils.run output, include_vars, files
end
