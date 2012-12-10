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

" Integer comparator used to sort the complete list of possible colors
function! s:IntCompare(i1, i2)
  return a:i1 == a:i2 ? 0 : a:i1 > a:i2 ? 1 : -1
endfunc

" Color comparator to find the nearest element to a given one in a given list
function! s:NearestElemInList(elem, list)
  let len = len(a:list)
  for i in range(len-1)
    if (a:elem <= (a:list[i] + a:list[i+1]) / 2)
      return a:list[i]
    endif
  endfor
  return a:list[len-1]
endfunction

" Takes 3 decimal values for r, g, and b, and returns the closest cube number.
"
" This approximator considers closeness based upon the individiual components.
" For each of r, g, and b, it finds the closest cube component available on
" the cube.  If the three closest matches can combine to form a valid color,
" this color is used, otherwise we repeat the search with the greys removed,
" meaning that the three new matches must make a valid color when combined.
function! csapprox#per_component#Approximate(r,g,b)
  let hex = printf("%02x%02x%02x", a:r, a:g, a:b)

  let colors = csapprox#common#Colors()
  let greys = csapprox#common#Greys()
  let type = csapprox#common#PaletteType()

  if !exists('s:approximator_cache_'.type)
    let s:approximator_cache_{type} = {}
  endif

  let rv = get(s:approximator_cache_{type}, hex, -1)
  if rv != -1
    return rv
  endif

  " Only obtain sorted list once
  if !exists("s:".type."_greys_colors")
    let s:{type}_greys_colors = sort(greys + colors, "s:IntCompare")
  endif

  let greys_colors = s:{type}_greys_colors

  let r = s:NearestElemInList(a:r, greys_colors)
  let g = s:NearestElemInList(a:g, greys_colors)
  let b = s:NearestElemInList(a:b, greys_colors)

  let len = len(colors)
  if (r == g && g == b && index(greys, r) != -1)
    let rv = 16 + len * len * len + index(greys, r)
  else
    let r = s:NearestElemInList(a:r, colors)
    let g = s:NearestElemInList(a:g, colors)
    let b = s:NearestElemInList(a:b, colors)
    let rv = index(colors, r) * len * len
         \ + index(colors, g) * len
         \ + index(colors, b)
         \ + 16
  endif

  let s:approximator_cache_{type}[hex] = rv
  return rv
endfunction
