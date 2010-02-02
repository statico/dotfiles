" FILE:     autoload/subprocess/proc_py.vim
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
"
scriptencoding utf-8

let s:lib = {}

function! subprocess#proc_py#import() "{{{
  let l:lib = deepcopy(s:lib, 1)
  return l:lib
endfunction "}}}

"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" API methods

function! s:lib.open(...) "{{{
    let command = get(a:000, 0, '')
    let env = get(a:000, 1, {})
    let env_string = '{'
    for k in keys(env)
        let env_string = env_string . "'" . s:python_escape(k) . "':'" . s:python_escape(env[k]) . "',"
    endfor
    let env_string = substitute(env_string, ',$', '', '') . '}'
    let b:subprocess_id = 'b' . string(localtime())
    silent execute ":python proc".b:subprocess_id." = proc_py()"
    silent execute ":python proc".b:subprocess_id.".open('" . s:python_escape(command) . "', " . env_string . ")"
endfunction "}}}

function! s:lib.read(...) "{{{
    let timeout = get(a:000, 0, '0.2')
    let b:proc_py_output = []
    silent execute ":python proc".b:subprocess_id.".read(" . string(timeout) . ")"
    return b:proc_py_output
endfunction "}}}

function! s:lib.write(command) "{{{
    silent execute ":python proc".b:subprocess_id.".write('" . s:python_escape(a:command) . "')"
endfunction "}}}

" Try to close process gracefully
" Linux signal 15, Windows close()
function! s:lib.close() "{{{
    silent execute ":python proc".b:subprocess_id.".close()"
endfunction "}}}

" Close process forcefully
" Linux signal 9, Windows close()
function! s:lib.kill() "{{{
    silent execute ":python proc".b:subprocess_id.".kill()"
endfunction "}}}

" Abandon process
" Linux signal 1, Windows close()
function! s:lib.hang_up() "{{{
    silent execute ":python proc".b:subprocess_id.".hang_up()"
endfunction "}}}

" Send an interrupt to process
" Typically <C-c>
function! s:lib.interrupt() "{{{
    silent execute ":python proc".b:subprocess_id.".interrupt()"
endfunction "}}}

" Update window size in kernel
function! s:lib.update_window_size(lines, cols) "{{{
    silent execute ":python proc" . b:subprocess_id . ".update_window_size(" . a:lines . "," . a:cols . ")"
endfunction "}}}

" Am I alive?
function! s:lib.get_status() "{{{
    let b:proc_py_status = 1
    silent execute ":python proc".b:subprocess_id.".get_status()"
    return b:proc_py_status
endfunction "}}}

" what library am I using to run the subprocess
function! s:lib.get_library_name() "{{{
    let b:proc_py_lib = 'unknown'
    silent execute ":python proc".b:subprocess_id.".get_library_name()"
    return b:proc_py_lib
endfunction "}}}

function! s:lib.get_env_var(var_name) "{{{
    let b:proc_py_env = ''
    silent execute ":python proc".b:subprocess_id.".get_env_var('" . s:python_escape(a:var_name) . "')"
    return b:proc_py_env
endfunction "}}}

"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Util

function! s:python_escape(str_in) "{{{
    let l:cleaned = a:str_in
    " newlines between python and vim are a mess
    let l:cleaned = substitute(l:cleaned, '\\', '\\\\', 'g')
    let l:cleaned = substitute(l:cleaned, '\n', '\\n', 'g')
    let l:cleaned = substitute(l:cleaned, '\r', '\\r', 'g')
    let l:cleaned = substitute(l:cleaned, "'", "\\\\'", 'g')
    return l:cleaned
endfunction "}}}

"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Python {{{
python << EOF

# Subprocess management in python.
# Uses 'pty' when possible 'popen' otherwise.
# 
# TODO: Beat interactive Windows commands into submission.
# TODO: Investigate win32pipe
#
# Heavily borrowed from vimsh.py <http://www.vim.org/scripts/script.php?script_id=165>

import vim, sys, os, string, signal, re, time

if sys.platform == 'win32' or sys.platform == 'win64':
    import popen2, stat
    use_pty = 0
else:
    import pty, tty, select, fcntl, termios, struct
    use_pty = 1


class proc_py:


    # constructor I guess (anything could be possible in python?)
    def __init__(self): # {{{
        self.buffer  = vim.current.buffer
        # }}}

    # create the pty or whatever (whatever == windows)
    def open(self, command, env = {}): # {{{
        command_arr  = command.split()
        self.command = command_arr[0]
        self.args    = command_arr

        # pty: praise jesus!
        if use_pty:

            try:
                self.pid, self.fd = pty.fork()
                #print self.pid
            except:
                print "pty.fork() failed. Did you mean pty.spork() ???"

            # child proc, replace with command after fucking with terminal attributes
            if self.pid == 0:

                # set requested environment variables
                for env_attr in env:
                    os.environ[env_attr] = env[env_attr]

                # set some attributes
                try:
                    attrs = tty.tcgetattr(1)
                    attrs[0] = attrs[0] ^ tty.IGNBRK
                    attrs[0] = attrs[0] | tty.BRKINT | tty.IXANY | tty.IMAXBEL
                    attrs[2] = attrs[2] | tty.HUPCL
                    attrs[3] = attrs[3] | tty.ICANON | tty.ECHO | tty.ISIG | tty.ECHOKE
                    attrs[6][tty.VMIN]  = 1
                    attrs[6][tty.VTIME] = 0
                    tty.tcsetattr(1, tty.TCSANOW, attrs)
                except:
                    pass

                os.execvp(self.command, self.args)

            # else master, pull termios settings and move on
            else:

                try:
                    attrs = tty.tcgetattr( 1 )
                    termios_keys = attrs[ 6 ]

                    self.eof_key   = termios_keys[ tty.VEOF ]
                    self.eol_key   = termios_keys[ tty.VEOL ]
                    self.erase_key = termios_keys[ tty.VERASE ]
                    self.intr_key  = termios_keys[ tty.VINTR ]
                    self.kill_key  = termios_keys[ tty.VKILL ]
                    self.susp_key  = termios_keys[ tty.VSUSP ]

                except:
                    pass

        # no pty, dagnabit
        else:
            # apparently pipes > popen2
            try:
                import win32pipe
                self.stdin, self.stdout, self.stderr = win32pipe.popen3(command)

            except ImportError:
                print 'w32 === fail!' # this always fails for me
                self.stdout, self.stdin, self.stderr = popen2.popen3(command, -1, 'b')

            self.outd = self.stdout.fileno()
            self.ind  = self.stdin.fileno ()
            self.errd = self.stderr.fileno()

            self.intr_key = ''
            self.eof_key  = ''
            # }}}

    # read from pty
    # XXX - select.poll() doesn't work in OS X!!!!!!!
    def read(self, timeout = 100): # {{{

        output = ''
        rtimeout = float(timeout) / 1000

        # score
        if use_pty:

            # what, no do/while?
            while 1:
                s_read, s_write, s_error = select.select( [ self.fd ], [], [], rtimeout)

                lines = ''
                for s_fd in s_read:
                    try:
                        lines = os.read( self.fd, 32 )
                    except:
                        pass
                    output = output + lines

                #self.buffer.append(str(output))
                if lines == '':
                    break

        # urk, windows
        else: 
            time.sleep(rtimeout)

            count = 0
            count = os.fstat(self.outd)[stat.ST_SIZE]

            while (count > 0):

                tmp = os.read(self.outd, 1)
                output += tmp

                count = os.fstat(self.outd)[stat.ST_SIZE]

                if len(tmp) == 0:
                    break

        #print output

        # XXX - BRUTAL
        lines_arr = re.split('\n', output)
        for v_line in lines_arr:
            cleaned = v_line
            cleaned = re.sub('\\\\', '\\\\\\\\', cleaned) # ftw!
            cleaned = re.sub('"', '\\\\"', cleaned)
            command = 'call add(b:proc_py_output, "' + cleaned + '")'
            #self.buffer.append(command)
            #self.buffer.append('')
            vim.command(command)

        return 
        # }}}

    # I guess this one's not bad
    def write(self, command): # {{{
        if use_pty:
            os.write(self.fd, command)
        else:
            os.write(self.ind, command)
        # }}}

    def close(self): # {{{
        if use_pty:
            os.kill(self.pid, signal.SIGTERM)
        else:
            os.close(self.ind)
            os.close(self.outd)
            os.close(self.errd)
        # }}}

    def kill(self): # {{{
        if use_pty:
            os.kill(self.pid, signal.SIGKILL)
        else:
            os.close(self.ind)
            os.close(self.outd)
            os.close(self.errd)
        # }}}

    def hang_up(self): # {{{
        if use_pty:
            os.kill(self.pid, signal.SIGHUP)
        else:
            os.close(self.ind)
            os.close(self.outd)
            os.close(self.errd)
        # }}}

    def interrupt(self): # {{{
        self.write(self.intr_key)
        # }}}

    # TODO: windows
    def get_status(self): #{{{

        p_status = 1 # boooooooooooooooooogus

        if use_pty:
            try:
                if os.waitpid( self.pid, os.WNOHANG )[0]:
                    p_status = 0
                else:
                    p_status = 1
            except:
                p_status = 0

        command = 'let b:proc_py_status = ' + str(p_status)
        vim.command(command)
        # }}}

    def get_library_name(self): #{{{

        if use_pty:
            command = 'let b:proc_py_lib = "pty"'
        else:
            command = 'let b:proc_py_lib = "popen"'

        vim.command(command)
        # }}}


    # XXX - ew
    def get_env_var(self, var_name): #{{{
        #env_val = ''
        #try:
        #    from ctypes import CDLL, c_char_p
        #    getenv = CDLL("libc.so.6").getenv
        #    getenv.restype = c_char_p
        #    env_val = getenv(var_name)
        #except:
        env_val = os.environ[var_name]

        command = 'let b:proc_py_env = "' + env_val + '"'
        vim.command(command)
        # }}}


    # update window size in kernel, then send SIGWINCH to fg process
    def update_window_size(self, rows, cols): # {{{
        if use_pty:
            try:
                fcntl.ioctl(self.fd, termios.TIOCSWINSZ, struct.pack("HHHH", rows, cols, 0, 0))
                os.kill(self.pid, signal.SIGWINCH)
            except:
                pass

        # }}}




EOF

"}}}

