
from time import sleep
import cProfile
import pstats

import vim

def conque_profile():

    terminal = ConqueTerm_1

    # start off by doing nothing for 5 seconds
    conque_profile_idle(terminal, 5)

    # show directory listing
    terminal.write("l")
    terminal.write("s")
    conque_profile_idle(terminal, 1)
    terminal.write("\r")
    conque_profile_idle(terminal, 1)

    # trigger an error
    terminal.write("n")
    terminal.write("o")
    terminal.write("t")
    conque_profile_idle(terminal, 1)
    terminal.write("o")
    terminal.write("t")
    terminal.write("t")
    terminal.write("t")
    terminal.write("t")
    terminal.write("t")
    terminal.write("t")
    terminal.write("t")
    terminal.write("t")
    terminal.write("o")
    terminal.write("t")
    terminal.write("t")
    terminal.write("d\r")
    conque_profile_idle(terminal, 2)
    
    # hit return key a bunch of times
    terminal.write("\r\r\r\r\r")
    conque_profile_idle(terminal, 0.5)
    terminal.write("\r\r\r\r\r")
    conque_profile_idle(terminal, 0.5)
    terminal.write("l")
    terminal.write("s")
    conque_profile_idle(terminal, 1)
    terminal.write("\r")
    terminal.write("\r\r\r\r\r")
    conque_profile_idle(terminal, 0.5)
    
    # end by doing nothing for 5 seconds
    conque_profile_idle(terminal, 5)



def conque_profile_idle(terminal, seconds):

    loops = seconds * 20

    for i in range(0, int(loops)):
        terminal.auto_read()
        sleep(0.050)


cProfile.run('conque_profile()', 'conque_profile_output')
p = pstats.Stats('conque_profile_output')
p.sort_stats('cumulative').print_stats(25)
    

