" FILE:     autoload/conque.vim
" AUTHOR:   Nico Raffo <nicoraffo@gmail.com>
"           Shougo Matsushita <Shougo.Matsu@gmail.com> (original VimShell)
"           Yukihiro Nakadaira (vimproc)
" MODIFIED: 2009-12-17
" VERSION:  0.6, for Vim 7.0
" LICENSE: {{{
" Conque - pty interaction in Vim
" Copyright (C) 2009 Nico Raffo 
"
" MIT License
" 
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

" Open a command in Conque.
" This is the root function that is called from Vim to start up Conque.
function! conque#open(...) "{{{
    let command = get(a:000, 0, '')
    let hooks   = get(a:000, 1, [])




    " bare minimum validation
    if empty(command)
        echohl WarningMsg | echomsg "No command found" | echohl None

        return 0
    else
        let l:cargs = split(command, '\s')
        if !executable(l:cargs[0])
            echohl WarningMsg | echomsg "Not an executable" | echohl None

            return 0
        endif
    endif

    " configure shell buffer display and key mappings
    call s:set_buffer_settings(command, hooks)

    " set global environment variables
    let $COLUMNS = winwidth(0) - 8
    let $LINES = winheight(0)
    let b:COLUMNS = $COLUMNS
    let b:LINES = $LINES

    " open command
    try
        let b:subprocess = subprocess#new()
        call b:subprocess.open(command, {'TERM': g:Conque_TERM, 'CONQUE': 1, 'EDITOR': 'unsupported'})

    catch 
        echohl WarningMsg | echomsg "Unable to open command: " . command | echohl None
        return 0
    endtry

    " init variables.
    let b:prompt_history = {}
    let b:auto_wrapped = 0
    let b:current_command = ''
    let b:write_clear = 0

    " save this buffer info
    let g:Conque_BufNr = bufnr("%")
    let g:Conque_BufName = bufname("%")
    let g:Conque_Idx += 1

    " read welcome message from command, give it a full second to start up
    call conque#read(500)



    startinsert!
    return 1
endfunction "}}}

" buffer settings, layout, key mappings, and auto commands
function! s:set_buffer_settings(command, pre_hooks) "{{{
    " optional hooks to execute, e.g. 'split'
    for h in a:pre_hooks
        silent execute h
    endfor

    silent execute "edit " . substitute(a:command, ' ', '\\ ', 'g') . "\\ -\\ " . g:Conque_Idx
    setlocal buftype=nofile  " this buffer is not a file, you can't save it
    setlocal nonumber        " hide line numbers
    setlocal foldcolumn=0    " reasonable left margin
    setlocal nowrap          " default to no wrap (esp with MySQL)
    setlocal noswapfile      " don't bother creating a .swp file
    set scrolloff=0          " don't use buffer lines. it makes the 'clear' command not work as expected
    setfiletype conque       " useful
    silent execute "setlocal syntax=".g:Conque_Syntax
    setlocal foldmethod=manual

    " run the current command
    nnoremap <buffer><silent><CR>        :<C-u>call conque#run()<CR>
    inoremap <buffer><silent><CR>        <ESC>:<C-u>call conque#run()<CR>
    nnoremap <buffer><silent><C-j>       <ESC>:<C-u>call conque#run()<CR>
    inoremap <buffer><silent><C-j>       <ESC>:<C-u>call conque#run()<CR>
    " don't backspace over prompt
    inoremap <buffer><silent><expr><BS>  <SID>delete_backword_char()
    " clear current line
    inoremap <buffer><silent><C-u>       <ESC>:<C-u>call conque#kill_line()<CR>
    " tab complete
    inoremap <buffer><silent><Tab>       <ESC>:<C-u>call <SID>tab_complete()<CR>
    nnoremap <buffer><silent><Tab>       <ESC>:<C-u>call <SID>tab_complete()<CR>
    " previous/next command
    inoremap <buffer><silent><Up>        <ESC>:<C-u>call <SID>previous_command()<CR>
    inoremap <buffer><silent><Down>      <ESC>:<C-u>call <SID>next_command()<CR>
    inoremap <buffer><silent><C-p>       <ESC>:<C-u>call <SID>previous_command()<CR>
    inoremap <buffer><silent><C-n>       <ESC>:<C-u>call <SID>next_command()<CR>
    " interrupt
    nnoremap <buffer><silent><C-c>       :<C-u>call conque#sigint()<CR>
    inoremap <buffer><silent><C-c>       <ESC>:<C-u>call conque#sigint()<CR>
    " escape
    nnoremap <buffer><silent><C-e>       :<C-u>call conque#escape()<CR>
    inoremap <buffer><silent><C-e>       <ESC>:<C-u>call conque#escape()<CR>
    " eof
    nnoremap <buffer><silent><C-d>       :<C-u>call conque#eof()<CR>
    inoremap <buffer><silent><C-d>       <ESC>:<C-u>call conque#eof()<CR>
    " suspend
    nnoremap <buffer><silent><C-z>       :<C-u>call conque#suspend()<CR>
    inoremap <buffer><silent><C-z>       <ESC>:<C-u>call conque#suspend()<CR>
    " quit
    nnoremap <buffer><silent><C-\>       :<C-u>call conque#quit()<CR>
    inoremap <buffer><silent><C-\>       <ESC>:<C-u>call conque#quit()<CR>
    " clear
    nnoremap <buffer><silent><C-l>       :<C-u>call conque#send('clear')<CR>
    inoremap <buffer><silent><C-l>       <ESC>:<C-u>call conque#send('clear')<CR>
    " inject selected text into conque
	  vnoremap <silent> <F9> :<C-u>call conque#inject(visualmode(), 0)<CR>

    " handle unexpected closing of shell
    " passes HUP to main and all child processes
    augroup conque
        autocmd BufUnload <buffer>   call conque#hang_up()
    augroup END
endfunction "}}}

" controller to execute current line
function! conque#run() "{{{



    " if we are not at the current active command line, don't execute
    if line('.') < max(keys(b:prompt_history)) && line('.') != line('$')
        execute "normal j^"
        return
    endif

    " check if subprocess still exists
    if !exists('b:subprocess')
        return
    endif



    " write current line to subprocess
    let l:write_status = conque#write(1)

    " if write was successful, read output
    if l:write_status == 1
        call conque#read(g:Conque_Read_Timeout)
        " special case
        if b:current_command == 'clear'
            normal Gzt
            startinsert!
        endif
    endif



endfunction "}}}

" write current line to pty
function! conque#write(add_newline) "{{{



    " pull command from the buffer
    let l:in = s:get_command()

    " check for hijacked commands
    if a:add_newline && (l:in =~ '^man ' || l:in =~ '^vim\? ')
        call conque#special(l:in)
        return 0
    endif
    
    " run the command!
    try

        if a:add_newline == 1

            call s:subwrite(l:in . "\<NL>")
        else

            call s:subwrite(l:in)
        endif
    catch

        echohl WarningMsg | echomsg 'No process' | echohl None
        call conque#exit()
        return 0
    endtry
    
    " record command history
    let b:current_command = l:in

    " clear out our command
    if exists("b:prompt_history['".line('.')."']")

        call setline(line('.'), b:prompt_history[line('.')])
    endif

    normal! $


    return 1
endfunction "}}}

" when we actually write a full command to the subprocess, occasionally we need to clear the input line first
" typically after command editing keystrokes such as for tab completion and history navigation
" XXX - hacky
function! s:subwrite(command) "{{{

    if b:write_clear == 1 && b:subprocess.get_library_name() == 'pty'

        call conque#kill_line()
        let b:write_clear = 0
    endif
    call b:subprocess.write(a:command)

endfunction "}}}

" parse current line to remove prompt and return command.
" also manages multi-line commands.
function! s:get_command() "{{{

    let l:in = getline('.')

    if l:in == ''
        " Do nothing.

    elseif exists("b:prompt_history['".line('.')."']")

        let l:in = l:in[len(b:prompt_history[line('.')]) : ]

    else
        " Maybe line numbering got disrupted, search for a matching prompt.
        let l:prompt_search = 0
        if line('.') == line('$')
            for pnr in reverse(sort(keys(b:prompt_history)))
                let l:prompt_length = len(b:prompt_history[pnr])
                " In theory 0 length or ' ' prompt shouldn't exist, but still...
                if l:prompt_length > 0 && b:prompt_history[pnr] != ' '
                    " Does the current line have this prompt?
                    if l:in[0 : l:prompt_length - 1] == b:prompt_history[pnr]
                        " found a matching prompt in history 

                        let b:prompt_history[line('.')] = b:prompt_history[pnr]
                        let l:in = l:in[l:prompt_length : ]
                        let l:prompt_search = pnr
                    endif
                endif
            endfor
        endif

        " Still nothing? Maybe a multi-line command was pasted in.
        let l:max_prompt = max(keys(b:prompt_history)) " Only count once.
        if l:prompt_search == 0 && l:max_prompt < line('$')

            for i in range(l:max_prompt, line('$'))
                if i == l:max_prompt
                    let l:in = getline(i)
                    let l:in = l:in[len(b:prompt_history[i]) : ]
                else

                    " detect if multi-line command was a product of command editing functions
                    if b:auto_wrapped == 1
                        let l:in = l:in . getline(i)
                    else
                        let l:in = l:in . "\n" . getline(i)
                    endif
                endif
            endfor
            call cursor(l:max_prompt, len(b:prompt_history[l:max_prompt]))
            let l:prompt_search = l:max_prompt

            " delete extra lines
            execute (l:prompt_search + 1) . ',' . line('$') . 'd'
        endif

        " Still nothing? We give up.
        if l:prompt_search == 0

            echohl WarningMsg | echo "Invalid input." | echohl None
            startinsert!
            return
        endif
    endif


    return l:in
endfunction "}}}

" read from pty and write to buffer
function! conque#read(timeout) "{{{




    try
        let l:output = b:subprocess.read(a:timeout)
    catch

        echohl WarningMsg | echomsg 'no process' | echohl None
        call conque#exit()
        return
    endtry




    if len(l:output) > 1 || l:output[0] != ''
        call s:print_buffer(l:output)
    endif



    " record prompt used on this line
    let b:prompt_history[line('.')] = getline('.')

    startinsert!



endfunction "}}}

" read from pty and return output as string
function! conque#read_return_raw(timeout) "{{{


    try
        let l:output = b:subprocess.read(a:timeout)
    catch

        echohl WarningMsg | echomsg 'no process' | echohl None
        call conque#exit()
        return
    endtry

    " ready to insert now


    return l:output
endfunction "}}}

" parse output from pty and update buffer
function! s:print_buffer(read_lines) "{{{


    call subprocess#shell_translate#process_input(line('.'), len(getline('.')) + 1, a:read_lines, 0)

    redraw



endfunction "}}}

function! conque#on_exit() "{{{

    augroup conque 
        autocmd! * <buffer>
    augroup END

    unlet b:subprocess


endfunction "}}}

" kill process pid with SIGTERM
" since most shells ignore SIGTERM there's a good chance this will do nothing
function! conque#exit() "{{{


    if b:subprocess.get_status() == 1
        " Kill process.
        try
            " 15 == SIGTERM
            call b:subprocess.close()
        catch /No such process/
        endtry
    endif

    call append(line('$'), '*Exit*')
    call conque#on_exit()
    normal G

endfunction "}}}

" kill process pid with SIGKILL
" undesirable, but effective
function! conque#force_exit() "{{{


    if b:subprocess.get_status() == 1
        " Kill processes.
        try
            " 9 == SIGKILL
            call b:subprocess.kill()
            call append(line('$'), '*Killed*')
        catch /No such process/
        endtry
    endif

    call conque#on_exit()
    normal G

endfunction "}}}

" kill process pid with SIGHUP
" this gets called if the buffer is unloaded before the program has been exited
" it should pass the signall to all children before killing the parent process
function! conque#hang_up() "{{{


    if !exists('b:subprocess')
        return
    endif

    if b:subprocess.get_status() == 1
        " Kill processes.
        try
            " 1 == HUP
            call b:subprocess.hang_up()
            call append(line('$'), '*Killed*')
        catch /No such process/
        endtry
    endif


    call conque#on_exit()
endfunction "}}}

" process command editing key strokes. History and tab completion being the most common.
function! s:process_command_edit(char) "{{{

    let l:prompt_line = max(keys(b:prompt_history))
    let l:prompt = b:prompt_history[l:prompt_line]
    let l:working_line = getline('.')
    let l:working_command = l:working_line[len(l:prompt) : len(l:working_line)]

    if b:write_clear == 1 && l:working_command == b:edit_command
        call b:subprocess.write(a:char)
    elseif b:write_clear == 0

        call b:subprocess.write(l:working_command . a:char)
        call setline(line('.'), l:prompt)
    elseif l:working_command[0 : len(b:edit_command) - 1] == b:edit_command

        call b:subprocess.write(l:working_command[len(b:edit_command) : ] . a:char)
        call setline(line('.'), l:prompt . b:edit_command)
    else


        call b:subprocess.write("\<C-u>" . l:working_command . a:char)
        call setline(line('.'), l:prompt . b:edit_command)
    endif
    let l:resp = conque#read_return_raw(g:Conque_Tab_Timeout)


    call subprocess#shell_translate#process_input(line('.'), len(getline('.')) + 1, l:resp, 1)




    "let b:prompt_history[l:prompt_line] = l:prompt

    let l:working_line = getline('.')
    let b:edit_command = l:working_line[len(l:prompt) : ]

    let b:write_clear = 1
    return
endfunction " }}}

" load previous command
function! s:previous_command() "{{{
    call s:process_command_edit("\e[A")
endfunction "}}}

" load next command
function! s:next_command() "{{{
    call s:process_command_edit("\e[B")
endfunction "}}}

" catch <BS> to prevent deleting prompt
" if tab completion has initiated, prevent deleting partial command already sent to pty
function! s:delete_backword_char() "{{{
    " identify prompt
    if exists('b:prompt_history[line(".")]')
        let l:prompt = b:prompt_history[line('.')]
    else
        return "\<BS>"
    endif
    
    if getline(line('.')) != l:prompt
        return "\<BS>"
    else
        return ""
    endif
endfunction "}}}

" tab complete current line
function! s:tab_complete() "{{{
    " pull more data first
    if exists("b:prompt_history[".line('.')."]") && b:prompt_history[line('.')] == getline(line('.'))
        call conque#read(5)
        return
    endif

    call s:process_command_edit("\<C-i>")
endfunction "}}}

" implement <C-u>
" especially useful to clear a tab completion line already sent to pty
function! conque#kill_line() "{{{
    " send <C-u> to pty
    try
        call b:subprocess.write("\<C-u>")
    catch

        echohl WarningMsg | echomsg 'no process' | echohl None
        call conque#exit()
        return
    endtry

    " we are throwing away the output here, assuming <C-u> never fails to do as expected
    let l:hopefully_just_backspaces = conque#read_return_raw(g:Conque_Tab_Timeout)

    " restore empty prompt
    let l:max_prompt = max(keys(b:prompt_history))
    call setline(l:max_prompt, b:prompt_history[l:max_prompt])
    if line('$') > l:max_prompt
        for i in range(l:max_prompt + 1, line('$'))
            call setline(i, '')
        endfor
    endif
    "normal! G$
    "startinsert!
endfunction "}}}

" implement <C-c>
" should send SIGINT to proc
function! conque#sigint() "{{{

    " send <C-c> to pty
    try
        call b:subprocess.write("\<C-c>")
    catch

        echohl WarningMsg | echomsg 'no process' | echohl None
        call conque#exit()
        return
    endtry
    call conque#read(500)

endfunction "}}}

" implement <Esc>
" should send <Esc> to proc
" Useful if Vim is launched inside of conque
function! conque#escape() "{{{

    " send <Esc> to pty
    try
        call b:subprocess.write("\<Esc>")
    catch

        echohl WarningMsg | echomsg 'no process' | echohl None
        call conque#exit()
        return
    endtry
    call conque#read(500)

endfunction "}}}

" implement <C-z>
" should suspend foreground process
function! conque#suspend() "{{{

    " send <C-z> to pty
    try
        call b:subprocess.write("\<C-z>")
    catch

        echohl WarningMsg | echomsg 'no process' | echohl None
        call conque#exit()
        return
    endtry
    call conque#read(500)

endfunction "}}}

" implement <C-d>
" should send EOF
function! conque#eof() "{{{

    " send <C-d> to pty
    try
        call b:subprocess.write("\<C-d>")
    catch

        echohl WarningMsg | echomsg 'no process' | echohl None
        call conque#exit()
        return
    endtry
    call conque#read(500)

endfunction "}}}

" implement <C-\>
" should send QUIT
function! conque#quit() "{{{

    " send <C-\> to pty
    try
        call b:subprocess.write("\<C-\\>")
    catch

        echohl WarningMsg | echomsg 'no process' | echohl None
        call conque#exit()
        return
    endtry
    call conque#read(500)

endfunction "}}}

" commands with special implementations
function! conque#special(command) "{{{
    call append(line('$'), b:prompt_history[max(keys(b:prompt_history))])
    normal G$

    if a:command =~ '^man '
        let split_cmd = "split " . substitute(a:command, '\W', '_', 'g')

        silent execute split_cmd
        setlocal buftype=nofile
        setlocal nonumber
        setlocal noswapfile
        let cmd = 'read !' . substitute(a:command, '^man ', 'man -P cat ', '')

        silent execute cmd

        " strip backspaces out of output
        try
            while search('\b', 'wncp') > 0
                silent execute '%s/[^\b]\b//g'
                silent execute '%s/^\b//g'
            endwhile
        catch
        endtry
        normal gg0
    elseif a:command =~ '^vim\? '
        let filename = substitute(a:command, '^vim\? ', '', '')
        let filename = b:subprocess.get_env_var('PWD') . '/' . filename
        let split_cmd = "split " . filename

        silent execute split_cmd
    endif
endfunction "}}}

" inject command from another buffer
function! conque#inject(type, execute) "{{{
    let reg_save = @@

    " yank current selection
    silent execute "normal! `<" . a:type . "`>y"

    let @@ = substitute(@@, '^[\r\n]*', '', '')
    let @@ = substitute(@@, '[\r\n]*$', '', '')

    silent execute ":sb " . g:Conque_BufName
    normal! G$p
    normal! G$
    startinsert!

    if a:execute == 1
        call conque#run()
    endif

    let @@ = reg_save
endfunction "}}}

" write command to conque
function! conque#send(command) "{{{
    call setline(line('$'), getline(line('$')) . a:command)
    call conque#run()
endfunction "}}}


" vim: foldmethod=marker
