#!/usr/bin/env python
# Copyright 2007 Giovanni Giorgi <jj@objectsroot.com>
# This program is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 2, or (at your option)
# any later version.

from Pymacs import lisp
import time

# Utility support functions
class EmacsLog:
    def __init__(self,category):
        self.logBuffer="*LogBuffer*" # "*Pymacs Log Buffer*"
        self.category=category
        self.startTime=time.time()

    def show(self,level,msg):
        start=int(time.time()-self.startTime)
        mx=str(start)+" <"+level+"> PY "+self.category+" "+msg
        lisp.message(mx)
        #mx = mx + "\n"
        #lisp.set_buffer(self.logBuffer)
        #lisp.insert(mx)
        
    def info(self, msg):
        self.show("I",msg)
        
    def debug(self,msg):
        self.show("DEBUG",msg)
        

    # Finest debugging
    def debugf(self,msg):
        self.show("DEBUG FINER",msg)


class BufferMan:
    def __init__(self):
        self.bufferName=lisp.buffer_name()
        self.fname=lisp.buffer_file_name()
        
    def getBufferAsText(self):
        f=open(self.fname,"r")
        text=f.read()
        f.close()
        return text
    
    def writeBuffer(self,text):
        f=open(self.fname,"w")
        f.write(text)
        f.close()
        self.reloadBuffer()
        
    def reloadBuffer(self):
        # ;; (switch-to-buffer bname)
        # ;; (revert-buffer 'IGNORE-AUTO 'NOCONFIRM)
        lisp.switch_to_buffer(self.bufferName)
        lisp.revert_buffer(lisp.IGNORE_AUTO, lisp.NOCONFIRM)





log=EmacsLog("main")
log.debugf("Pymacs.utility Loaded and happy to serve you")

