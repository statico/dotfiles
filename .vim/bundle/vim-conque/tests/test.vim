
function! AssertEquals(a, b) " {{{
    if a:a != a:b
        throw "Assertion (equals) failed: " . a:a . " = " . a:b
    endif
endfunction " }}}

function! AssertTrue(a) " {{{
    if !a:a
        throw "Assertion (true) failed: " . a:a
    endif
endfunction " }}}

function! AssertFalse(a) " {{{
    if a:a
        throw "Assertion (false) failed: " . a:a
    endif
endfunction " }}}

