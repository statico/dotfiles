so test.vim

" test terminal

"let sub = conque_term#open('bash')
"let output = sub.read(2000)
"call AssertEquals('nraffo@nraffo-laptop:~/.vim/tests$ ', output)
"call sub.close()

" test subprocess

"let sub = conque_term#subprocess('whoami')
"let output = sub.read(500)
"call AssertEquals('nraffo', output)
"call sub.close()

" test callback

function! MyCB(output)
    call append(line('$'), a:output)
endfunction

let sub = conque_term#subprocess('bash')
call sub.set_callback('MyCB')
call sub.writeln('pwd')

