"
" Source this file in a new buffer.
" Expected results:
" 
"   1. Letters typed in insert mode are capitalized
"   2. Spaces are removed
"

if exists('##InsertCharPre')
    echo 'InsertCharPre event exists'
else
    echo 'InsertCharPre event does not exist'
    finish
endif

function! MyCap()
    if v:char == ' '
        sil let v:char = ''
    else
        sil let v:char = toupper(v:char)
    endif
endfunction

autocmd InsertCharPre <buffer> call MyCap()

startinsert!

"call feedkeys("a b c\n", "t")
"call feedkeys("ᕕ ᕈ ᓗ \n", "t")

"echo "ᕕ ᕈ ᓗ "
"echo "a b c"

