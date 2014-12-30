if exists("g:loaded_coffee_autotag")
  finish
endif

let g:loaded_coffee_autotag=1

if !has("ruby")
  echohl WarningMsg
  echo "Coffee auto tag requires Vim to be compiled with Ruby support"
  echohl none
  finish
endif

let s:CoffeeAutoTagFile="./tags"
let s:CoffeeAutoTagIncludeVars=0
let s:CoffeeAutoTagTagRelative=1

if !exists("g:CoffeeAutoTagDisabled")
  let g:CoffeeAutoTagDisabled = 0
endif

if exists("g:CoffeeAutoTagFile")
  let s:CoffeeAutoTagFile = g:CoffeeAutoTagFile
endif

if exists("g:CoffeeAutoTagIncludeVars")
  let s:CoffeeAutoTagIncludeVars = g:CoffeeAutoTagIncludeVars
endif

if exists("g:CoffeeAutoTagTagRelative")
  let s:CoffeeAutoTagTagRelative = g:CoffeeAutoTagTagRelative
endif

if s:CoffeeAutoTagIncludeVars
  let s:raw_args="--include-vars"
else
  let s:raw_args=""
endif

let g:tagbar_type_coffee = {
      \   'ctagsbin' : 'coffeetags',
      \   'ctagsargs' : s:raw_args,
      \   'kinds' : [
      \     'f:functions',
      \     'c:classes',
      \     'o:object',
      \     'v:variables',
      \     'p:prototypes',
      \     'b:blocks'
      \   ],
      \   'sro' : ".",
      \   'kind2scope' : {
      \     'f' : 'object',
      \     'o' : 'object',
      \   }
      \ }


function! CoffeeAutoTag()
  if g:CoffeeAutoTagDisabled
    return
  endif

  let cmd = 'coffeetags --append -f ' . s:CoffeeAutoTagFile . ' '

  if s:CoffeeAutoTagIncludeVars
    let cmd .= '--include-vars '
  endif

  if s:CoffeeAutoTagTagRelative
    let cmd .= '--tag-relative '
  endif

  let cmd .= expand("%:p")

  let output = system(cmd)

  if exists(":TlistUpdate")
    TlistUpdate
  endif
endfunction

augroup CoffeeAutoTag
  au!
  autocmd BufWritePost,FileWritePost *.coffee call CoffeeAutoTag()
augroup END
