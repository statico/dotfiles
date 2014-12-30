###
Lol wut
echo : ->
echoes
###

echo2 :-> console.log 'echo'

### One line blockcomment ###
foo2 : (x) -> console.log 'bar #{x}'

###
not so well formatted blockcomment
echo4 :-> console.log 'echo4' ###
baz : (x, y) ->
  console.log 'baz #{x} : #{y}'

###
well formatted
block comment
###

echo3 :-> console.log 'echo'

### One line blockcomment ###
foo : (x) -> console.log 'bar #{x}'
