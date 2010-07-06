import xmlrpclib
from SimpleXMLRPCServer import SimpleXMLRPCServer
from Pymacs import lisp
import time
interactions={}


closeConnectionFlag=False

def publish_XMLRPC_Server():
    lisp.switch_to_buffer("*Messages*")
    # lisp.delete_other_window()
    lisp.message("XML RPC Started on port 9000. Ctrl-G to stop")
    server=SimpleXMLRPCServer(("127.0.0.1",9000))
    # We will allow dotted names:
    server.register_function(closeConnection,"closeConnection")
    server.register_instance(lisp, True)
    
    #server.serve_forever()
    globals()['closeConnectionFlag']=False
    
    while globals()['closeConnectionFlag']==False:
        server.handle_request()

    lisp.message("Connection closed")
    
# Support function: used to
# stop serving:
def closeConnection():
    globals()['closeConnectionFlag']=True
    return 1

interactions[publish_XMLRPC_Server]=''

