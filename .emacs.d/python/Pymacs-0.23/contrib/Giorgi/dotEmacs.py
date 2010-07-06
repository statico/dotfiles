# jjEmacsPythoned.py
#

import sys
import os.path
import string 
from Pymacs import lisp


sys.path.append(".")


interactions = {}


def debug(msg):
    msg = "<D> "+msg
    def t():
        lisp.message(msg)
        lisp.set_buffer("*LogBuffer*")
        lisp.goto_line(lisp.point_max())
        lisp.insert(msg+"\n")
    lisp.save_excursion(t)

def doMainConfig():
    # On emacs 22 enable cua mode:
    try:
        lisp.cua_mode(True)
        debug("Cua mode succesfully initialized")
    except Exception:
        debug("Failed Cua mode init")

def testExceptionFramework():
    try:
      lisp.give_me_an_error()
    except Protocol.ErrorException:
        debug("Errore get")


def initLogs():
    lisp.switch_to_buffer("*LogBuffer*")
    debug("Log Buffer succesfully initialized by Pymacs")
    


################ Callback for auto-reloading python modules if needed
def get_module_name_if_python_file_handle():
    fname=lisp.buffer_file_name()
    if fname==None:
        return None    
    # check it:
    if fname[-3:] == '.py' and (fname.endswith("pymacs.py")==False):
        # Ok, we have got something to do:
        # replace last / with a point and try it down:
        i=fname.rfind("/")
        pk=fname[:i]+"."+fname[i+1:-3]        
        #debug("Reloading "+pk)
        return pk
    else:
        #say(" Nothing to do for:"+fname)
        return None

interactions[get_module_name_if_python_file_handle]=''

#### BASIC SAVE HOOK always called:
def save_hook(bufferFileName):
    #say("Nothing to do")
    pass

interactions[save_hook]=''

def installPymacsMenu():
    pass


interactions[debug]=''
interactions[initLogs]=''
interactions[doMainConfig]=''
interactions[testExceptionFramework]=''
