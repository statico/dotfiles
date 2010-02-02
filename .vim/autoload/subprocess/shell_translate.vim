" FILE:     autoload/subprocess/shell_translate.vim
" AUTHOR:   Nico Raffo <nicoraffo@gmail.com>
" MODIFIED: 2009-12-17
" VERSION:  0.6, for Vim 7.0
" LICENSE:  MIT License "{{{
" Permission is hereby granted, free of charge, to any person obtaining a copy
" of this software and associated documentation files (the "Software"), to deal
" in the Software without restriction, including without limitation the rights
" to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
" copies of the Software, and to permit persons to whom the Software is
" furnished to do so, subject to the following conditions:
" 
" The above copyright notice and this permission notice shall be included in
" all copies or substantial portions of the Software.
" 
" THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
" IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
" FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
" AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
" LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
" OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
" THE SOFTWARE.
" }}}
" Comments {{{
" Translate shell escape/control characters into Vim formatting
"
" TODO: Refactor this entire script.
"
" The character-by-character processing is a resource hog, and almost certainly unnecessary, 
" since ALL of the sequences we pay attention to follow the same form ^[[(\d+;)*\d+\w
"
" It also may be helpful if we don't print lines to the buffer before processing begins. Meaning
" we'd pass an array of lines to this script along with a starting line and column. This would be 
" particularly helpful for input with many A/B jumps.
"
" TODO: Handle the ^[[\d;\dH (cursor to x/y) escape sequence.
"
" Moving the cursor to a specified x/y position on the screen may be possible. We should be able
" to make a judgement on where line 0 is in the pty by taking the larger number of the last 
" clear_screen or last printed line - $LINES.
"
" The only serious issue with this escape will be deciding what to do when the user begins typing
" on a different line than where the shell cursor should be. Going into full duplex mode would
" help, but that's another can of worms.
" }}}

"\ 'code':']0;.*'.nr2char(7), 'name':'title',
"\ 'code':'(.', 'name':'char_set',
"\ 'code':'[?\d*l', 'name':'cursor_settings',
"\ 'code':'[?\d*h', 'name':'cursor_settings',

" Escape sequences {{{
let s:escape_sequences = { 
\ 'm':'font',
\ 'J':'clear_screen',
\ 'K':'clear_line',
\ '@':'add_spaces',
\ 'A':'cursor_up',
\ 'B':'cursor_down',
\ 'C':'cursor_right',
\ 'D':'cursor_left',
\ 'G':'cursor_to_column',
\ 'H':'cursor',
\ 'L':'insert_lines',
\ 'M':'delete_lines',
\ 'P':'delete_chars',
\ 'd':'cusor_vpos',
\ 'f':'xy_pos',
\ 'g':'tab_clear'
\ } 
" }}}

" Font codes {{{
let s:font_codes = {
\ '0': {'description':'Normal (default)', 'attributes': {'cterm':'NONE','ctermfg':'NONE','ctermbg':'NONE','gui':'NONE','guifg':'NONE','guibg':'NONE'}, 'normal':1},
\ '00': {'description':'Normal (default) alternate', 'attributes': {'cterm':'NONE','ctermfg':'NONE','ctermbg':'NONE','gui':'NONE','guifg':'NONE','guibg':'NONE'}, 'normal':1},
\ '1': {'description':'Bold', 'attributes': {'cterm':'BOLD','gui':'BOLD'}, 'normal':0},
\ '01': {'description':'Bold', 'attributes': {'cterm':'BOLD','gui':'BOLD'}, 'normal':0},
\ '4': {'description':'Underlined', 'attributes': {'cterm':'UNDERLINE','gui':'UNDERLINE'}, 'normal':0},
\ '04': {'description':'Underlined', 'attributes': {'cterm':'UNDERLINE','gui':'UNDERLINE'}, 'normal':0},
\ '5': {'description':'Blink (appears as Bold)', 'attributes': {'cterm':'BOLD','gui':'BOLD'}, 'normal':0},
\ '05': {'description':'Blink (appears as Bold)', 'attributes': {'cterm':'BOLD','gui':'BOLD'}, 'normal':0},
\ '7': {'description':'Inverse', 'attributes': {'cterm':'REVERSE','gui':'REVERSE'}, 'normal':0},
\ '07': {'description':'Inverse', 'attributes': {'cterm':'REVERSE','gui':'REVERSE'}, 'normal':0},
\ '8': {'description':'Invisible (hidden)', 'attributes': {'ctermfg':'0','ctermbg':'0','guifg':'#000000','guibg':'#000000'}, 'normal':0},
\ '08': {'description':'Invisible (hidden)', 'attributes': {'ctermfg':'0','ctermbg':'0','guifg':'#000000','guibg':'#000000'}, 'normal':0},
\ '22': {'description':'Normal (neither bold nor faint)', 'attributes': {'cterm':'NONE','gui':'NONE'}, 'normal':1},
\ '24': {'description':'Not underlined', 'attributes': {'cterm':'NONE','gui':'NONE'}, 'normal':1},
\ '25': {'description':'Steady (not blinking)', 'attributes': {'cterm':'NONE','gui':'NONE'}, 'normal':1},
\ '27': {'description':'Positive (not inverse)', 'attributes': {'cterm':'NONE','gui':'NONE'}, 'normal':1},
\ '28': {'description':'Visible (not hidden)', 'attributes': {'ctermfg':'NONE','ctermbg':'NONE','guifg':'NONE','guibg':'NONE'}, 'normal':1},
\ '30': {'description':'Set foreground color to Black', 'attributes': {'ctermfg':'16','guifg':'#000000'}, 'normal':0},
\ '31': {'description':'Set foreground color to Red', 'attributes': {'ctermfg':'1','guifg':'#ff0000'}, 'normal':0},
\ '32': {'description':'Set foreground color to Green', 'attributes': {'ctermfg':'2','guifg':'#00ff00'}, 'normal':0},
\ '33': {'description':'Set foreground color to Yellow', 'attributes': {'ctermfg':'3','guifg':'#ffff00'}, 'normal':0},
\ '34': {'description':'Set foreground color to Blue', 'attributes': {'ctermfg':'4','guifg':'#0000ff'}, 'normal':0},
\ '35': {'description':'Set foreground color to Magenta', 'attributes': {'ctermfg':'5','guifg':'#990099'}, 'normal':0},
\ '36': {'description':'Set foreground color to Cyan', 'attributes': {'ctermfg':'6','guifg':'#009999'}, 'normal':0},
\ '37': {'description':'Set foreground color to White', 'attributes': {'ctermfg':'7','guifg':'#ffffff'}, 'normal':0},
\ '39': {'description':'Set foreground color to default (original)', 'attributes': {'ctermfg':'NONE','guifg':'NONE'}, 'normal':1},
\ '40': {'description':'Set background color to Black', 'attributes': {'ctermbg':'16','guibg':'#000000'}, 'normal':0},
\ '41': {'description':'Set background color to Red', 'attributes': {'ctermbg':'1','guibg':'#ff0000'}, 'normal':0},
\ '42': {'description':'Set background color to Green', 'attributes': {'ctermbg':'2','guibg':'#00ff00'}, 'normal':0},
\ '43': {'description':'Set background color to Yellow', 'attributes': {'ctermbg':'3','guibg':'#ffff00'}, 'normal':0},
\ '44': {'description':'Set background color to Blue', 'attributes': {'ctermbg':'4','guibg':'#0000ff'}, 'normal':0},
\ '45': {'description':'Set background color to Magenta', 'attributes': {'ctermbg':'5','guibg':'#990099'}, 'normal':0},
\ '46': {'description':'Set background color to Cyan', 'attributes': {'ctermbg':'6','guibg':'#009999'}, 'normal':0},
\ '47': {'description':'Set background color to White', 'attributes': {'ctermbg':'7','guibg':'#ffffff'}, 'normal':0},
\ '49': {'description':'Set background color to default (original).', 'attributes': {'ctermbg':'NONE','guibg':'NONE'}, 'normal':1},
\ '90': {'description':'Set foreground color to Black', 'attributes': {'ctermfg':'16','guifg':'#000000'}, 'normal':0},
\ '91': {'description':'Set foreground color to Red', 'attributes': {'ctermfg':'1','guifg':'#ff0000'}, 'normal':0},
\ '92': {'description':'Set foreground color to Green', 'attributes': {'ctermfg':'2','guifg':'#00ff00'}, 'normal':0},
\ '93': {'description':'Set foreground color to Yellow', 'attributes': {'ctermfg':'3','guifg':'#ffff00'}, 'normal':0},
\ '94': {'description':'Set foreground color to Blue', 'attributes': {'ctermfg':'4','guifg':'#0000ff'}, 'normal':0},
\ '95': {'description':'Set foreground color to Magenta', 'attributes': {'ctermfg':'5','guifg':'#990099'}, 'normal':0},
\ '96': {'description':'Set foreground color to Cyan', 'attributes': {'ctermfg':'6','guifg':'#009999'}, 'normal':0},
\ '97': {'description':'Set foreground color to White', 'attributes': {'ctermfg':'7','guifg':'#ffffff'}, 'normal':0},
\ '100': {'description':'Set background color to Black', 'attributes': {'ctermbg':'16','guibg':'#000000'}, 'normal':0},
\ '101': {'description':'Set background color to Red', 'attributes': {'ctermbg':'1','guibg':'#ff0000'}, 'normal':0},
\ '102': {'description':'Set background color to Green', 'attributes': {'ctermbg':'2','guibg':'#00ff00'}, 'normal':0},
\ '103': {'description':'Set background color to Yellow', 'attributes': {'ctermbg':'3','guibg':'#ffff00'}, 'normal':0},
\ '104': {'description':'Set background color to Blue', 'attributes': {'ctermbg':'4','guibg':'#0000ff'}, 'normal':0},
\ '105': {'description':'Set background color to Magenta', 'attributes': {'ctermbg':'5','guibg':'#990099'}, 'normal':0},
\ '106': {'description':'Set background color to Cyan', 'attributes': {'ctermbg':'6','guibg':'#009999'}, 'normal':0},
\ '107': {'description':'Set background color to White', 'attributes': {'ctermbg':'7','guibg':'#ffffff'}, 'normal':0}
\ } 
" }}}

" nr2char() is oddly more reliable than \r etc
let s:action_match = '\(\e[?\?\(\d\+;\)*\d*\(\w\|@\)\|'.nr2char(13).'\|'.nr2char(8).'\|'.nr2char(7).'\)'

" line break mode
let s:auto_wrap = 0

function! subprocess#shell_translate#process_input(line, col, input, auto_wrap) " {{{
    " don't want to pass these around in every function arg
    let s:line = a:line
    let s:col = a:col
    let s:auto_wrap = a:auto_wrap
    let b:auto_wrapped = 0
    
    for i in range(len(a:input))
        call subprocess#shell_translate#process_line(a:input[i], i == len(a:input) - 1 ? 0 : 1)
    endfor


    call cursor(s:line, s:col)
    startinsert!
endfunction " }}}


function! subprocess#shell_translate#process_line(input_line, add_newline) " {{{
    let l:line_pos = s:col - 1
    let l:input = a:input_line
    let l:output = getline(s:line)
    let l:color_changes = []



    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    " REMOVE REDUNDANT/IGNORED ESCAPE SEQUENCES. 
    " This often removes the requirement to parse the line char by char, which is a huge performance hit.

    if l:input =~ '\e' " {{{
        " remove trailing <CR>s. conque assumes cursor will be at col 0 for new lines
        "let l:input = substitute(l:input, '\r\+$', '', '')
        " remove character set escapes. they would be ignored
        let l:input = substitute(l:input, '\e(.', '', 'g')
        " remove initial color escape if it is setting color to normal. conque always starts lines in normal syntax
        let l:input = substitute(l:input, '^\(\e[0\?m\)*', '', '')
        " remove title changes
        let l:input = substitute(l:input, '\e]\d;.\{-\}'.nr2char(7), '', '')
        " remove trailing color escapes. syntax changes are limited to one line
        let l:input = substitute(l:input, '\(\e[\(\d*;\)*\d*m\)*$', '', '')
        " remove all normal color escapes leading up to the first non-normal color escape
        while l:input =~ '^[^\e]\+\e[\(39;49\|0\)\?m'

            let l:input = substitute(l:input, '\e[\(39;49\|0\)\?m', '', '')
        endwhile
    endif " }}}




    " ****************************************************************************************** "
    " Loop over action matches {{{
    let l:match_num = match(l:input, s:action_match)

    while l:match_num != -1
        " pack characters up to the match onto output
        if l:match_num > 0 && l:line_pos == 0
            let l:output =                                  l:input[ 0 : l:match_num - 1 ] . l:output[ l:line_pos + l:match_num : ]
        elseif l:match_num > 0
            let l:output = l:output[ 0 : l:line_pos - 1 ] . l:input[ 0 : l:match_num - 1 ] . l:output[ l:line_pos + l:match_num : ]
        endif

        " handle line wrapping
        if l:line_pos + l:match_num > b:COLUMNS && (s:auto_wrap == 1 || l:input[ l:match_num - 1 : ] =~ '\r.')

            let b:auto_wrapped = 1

            " break output at screen width
            "let l:input = nr2char(13) . l:output[ b:COLUMNS : ] . l:input[ l:match_num : ]
            let l:input = nr2char(13) . l:input[ l:match_num : ]
            let l:output = l:output[ : b:COLUMNS - 1 ]
            



            " finish off this line
            call setline(s:line, l:output)
            call s:process_colors(l:color_changes)
            call cursor(s:line, s:col)
            call winline()

            " initialize cursor in the correct position
            let s:line += 1
            let s:col = 1
            "call setline(s:line, '')

            " ship off the rest of input to next line
            call subprocess#shell_translate#process_line(l:input, a:add_newline)
            return
        endif



        let l:match_str = matchstr(l:input, s:action_match, l:match_num)


        let l:input = l:input[l:match_num + len(l:match_str) :]

        let l:line_pos += l:match_num


        if l:match_str == nr2char(8)

            let l:line_pos = l:line_pos - 1
        elseif l:match_str == nr2char(13)

            let l:line_pos = 0
        elseif l:match_str == nr2char(7)

            echohl WarningMsg | echomsg "BELL!" | echohl None
        else
            " last character
            let l:key = l:match_str[-1 : -1]

            if exists('s:escape_sequences[l:key]')


                " action tied to this last character
                let l:action = s:escape_sequences[l:key]
                " numeric modifiers
                let l:vals = split(l:match_str[2 : -2], ';')
                let l:delta = len(l:vals) > 0 ? l:vals[0] : 1


                " ********************************************************************************** "
                " Escape actions " {{{
                if l:action == 'font'
                    call add(l:color_changes, {'col': l:line_pos, 'codes': l:vals})

                elseif l:action == 'clear_line'
                    if line_pos == 0
                        let l:output = ''
                    else
                        let l:output = l:output[ : l:line_pos - 1]
                    endif

                elseif l:action == 'cursor_right'
                    let l:line_pos = l:line_pos + l:delta

                elseif l:action == 'cursor_left'
                    let l:line_pos = l:line_pos - l:delta

                elseif l:action == 'cursor_to_column'
                    let l:line_pos = l:delta - 1
                    while len(l:output) <= l:line_pos
                        let l:output = l:output . ' '
                    endwhile

                elseif l:action == 'cursor_up' " holy crap we're screwed
                    " finish off this line
                    call setline(s:line, l:output)
                    call s:process_colors(l:color_changes)
                    call cursor(s:line, s:col)
                    call winline()

                    " initialize cursor in the correct position
                    let s:line = s:line - l:delta
                    let s:col = l:line_pos + 1



                    " ship off the rest of input to next line
                    call subprocess#shell_translate#process_line(l:input, a:add_newline)
                    return

                elseif l:action == 'cursor_down' " holy crap we're screwed
                    " finish off this line
                    call setline(s:line, l:output)
                    call s:process_colors(l:color_changes)
                    call cursor(s:line, s:col)
                    call winline()

                    " initialize cursor in the correct position
                    let s:line = s:line + l:delta
                    let s:col = l:line_pos + 1



                    " ship off the rest of input to next line
                    call subprocess#shell_translate#process_line(l:input, a:add_newline)
                    return

                elseif l:action == 'clear_screen'
                    let l:line_pos = 0
                    let l:output = ''

                elseif l:action == 'delete_chars'
                    if l:line_pos == 0
                        let l:output =                               l:output[l:line_pos + l:delta : ]
                    else
                        let l:output = l:output[ : l:line_pos - 1] . l:output[l:line_pos + l:delta : ]
                    endif

                elseif l:action == 'add_spaces'


                    let l:spaces = []
                    for sp in range(l:delta)
                        call add(l:spaces, ' ')
                    endfor


                    if l:line_pos == 0
                        let l:output =                               join(l:spaces, '') . l:output[l:line_pos : ]
                    else
                        let l:output = l:output[ : l:line_pos - 1] . join(l:spaces, '') . l:output[l:line_pos : ]
                    endif
                endif
                " }}}

            endif

        endif

        let l:match_num = match(l:input, s:action_match)

    endwhile
    " }}}

    " pack on remaining input
    if l:line_pos > 0
        let l:output = l:output[ 0 : l:line_pos - 1 ] . l:input . l:output[ l:line_pos + len(l:input) : ]
    else
        let l:output =                                  l:input . l:output[ l:line_pos + len(l:input) : ]
    endif


    " handle line wrapping
    if len(l:output) > b:COLUMNS && (s:auto_wrap == 1 || l:input[ l:match_num - 1 : ] =~ '\r.')

        let b:auto_wrapped = 1

        " break output at screen width
        let l:input = nr2char(13) . l:output[ b:COLUMNS : ]
        let l:output = l:output[ : b:COLUMNS - 1 ]
        



        " finish off this line
        call setline(s:line, l:output)
        call s:process_colors(l:color_changes)
        call cursor(s:line, s:col)
        call winline()

        " initialize cursor in the correct position
        let s:line += 1
        let s:col = 1

        " ship off the rest of input to next line
        call subprocess#shell_translate#process_line(l:input, a:add_newline)
        return
    endif

    " strip trailing spaces
    let l:output = substitute(l:output, '\s\+$', '', '')
    let l:line_pos += len(l:input)

    if l:line_pos > len(l:output)
        let l:output = l:output . ' '
    endif

    " set line
    call setline(s:line, l:output)

    " color it
    call s:process_colors(l:color_changes)



    " create a new line if requested
    if a:add_newline == 1
        let s:line += 1
    endif

    let s:col = l:line_pos + 1

endfunction " }}}


function! s:process_colors(color_changes) " {{{
    if len(a:color_changes) == 0
        return
    endif

    " color it
    let l:hi_ct = 1
    let l:last_col = len(substitute(getline(s:line), '\s\+$', '', ''))
    for cc in reverse(a:color_changes)
        if cc.col > l:last_col + 2
            continue
        endif

        let l:highlight = ''
        for color_number in cc.codes
            if exists('s:font_codes['.color_number.']')
                for attr in keys(s:font_codes[color_number].attributes)
                    let l:highlight = l:highlight . ' ' . attr . '=' . s:font_codes[color_number].attributes[attr]
                endfor
            endif
        endfor

        let syntax_name = ' EscapeSequenceAt_' . bufnr('%') . '_' . s:line . '_' . l:hi_ct
        let syntax_region = 'syntax match ' . syntax_name . ' /\%' . s:line . 'l\%>' . cc.col . 'c.*\%<' . (l:last_col + 2) . 'c/ contains=ALL '
        "let syntax_link = 'highlight link ' . syntax_name . ' Normal'
        let syntax_highlight = 'highlight ' . syntax_name . l:highlight

        silent execute syntax_region
        "execute syntax_link
        silent execute syntax_highlight






        let l:hi_ct += 1
    endfor
endfunction " }}}



" vim: foldmethod=marker
