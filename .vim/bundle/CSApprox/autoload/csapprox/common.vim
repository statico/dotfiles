" Copyright (c) 2012, Matthew J. Wozniski
" All rights reserved.
"
" Redistribution and use in source and binary forms, with or without
" modification, are permitted provided that the following conditions are met:
"     * Redistributions of source code must retain the above copyright
"       notice, this list of conditions and the following disclaimer.
"     * Redistributions in binary form must reproduce the above copyright
"       notice, this list of conditions and the following disclaimer in the
"       documentation and/or other materials provided with the distribution.
"     * The names of the contributors may not be used to endorse or promote
"       products derived from this software without specific prior written
"       permission.
"
" THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDER ``AS IS'' AND ANY
" EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
" WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
" DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER BE LIABLE FOR ANY
" DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
" (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
" LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
" ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
" (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
" SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

let s:xterm_colors   = [ 0x00, 0x5F, 0x87, 0xAF, 0xD7, 0xFF ]
let s:eterm_colors   = [ 0x00, 0x2A, 0x55, 0x7F, 0xAA, 0xD4 ]
let s:konsole_colors = [ 0x00, 0x33, 0x66, 0x99, 0xCC, 0xFF ]
let s:xterm_greys    = [ 0x08, 0x12, 0x1C, 0x26, 0x30, 0x3A,
                       \ 0x44, 0x4E, 0x58, 0x62, 0x6C, 0x76,
                       \ 0x80, 0x8A, 0x94, 0x9E, 0xA8, 0xB2,
                       \ 0xBC, 0xC6, 0xD0, 0xDA, 0xE4, 0xEE ]

let s:urxvt_colors   = [ 0x00, 0x8B, 0xCD, 0xFF ]
let s:urxvt_greys    = [ 0x2E, 0x5C, 0x73, 0x8B,
                       \ 0xA2, 0xB9, 0xD0, 0xE7 ]

" Uses &term to determine which cube should be use.  If &term is set to
" "xterm" or begins with "screen", the variables g:CSApprox_eterm and
" g:CSApprox_konsole can be used to select a different palette.
function! csapprox#common#PaletteType()
  if &t_Co == 88
    let type = 'urxvt'
  elseif &term ==# 'xterm' || &term =~# '^screen' || &term==# 'builtin_gui'
    if exists('g:CSApprox_konsole') && g:CSApprox_konsole
      let type = 'konsole'
    elseif exists('g:CSApprox_eterm') && g:CSApprox_eterm
      let type = 'eterm'
    else
      let type = 'xterm'
    endif
  elseif &term =~? '^konsole'
    " Konsole only used its own palette up til KDE 4.2.0
    if executable('kde4-config') && system('kde4-config --kde-version') =~ '^4\.[10]\.'
      let type = 'konsole'
    elseif executable('kde-config') && system('kde-config --version') =~# 'KDE: 3\.'
      let type = 'konsole'
    else
      let type = 'xterm'
    endif
  elseif &term =~? '^eterm'
    let type = 'eterm'
  else
    let type = 'xterm'
  endif

  return type
endfunction

" Retrieve the list of greyscale ramp colors for the current palette
function! csapprox#common#Greys()
  return (&t_Co == 88 ? s:urxvt_greys : s:xterm_greys)
endfunction

" Retrieve the list of non-greyscale ramp colors for the current palette
function! csapprox#common#Colors()
  return s:{csapprox#common#PaletteType()}_colors
endfunction
