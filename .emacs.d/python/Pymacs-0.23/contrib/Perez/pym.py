# -*- coding: utf-8 -*-
"""Functions accessible from inside Emacs.

Filename: pym.py

Usage in Emacs: import them with
M-x pymacs-load pym

And then all functions here become available in Emacs as
pym-function-name with every _ replaced by - in the names.
"""

__author__ = "Fernando Perez. <fperez@pizero.colorado.edu>"
__license__= "GPL"

import re
from Pymacs import pymacs

# emacs is the global which handles lisp-based interaction with the active
# Emacs buffer from which any function is called

emacs = pymacs.lisp

# interactions is a global dict which MUST be updated for each new function
# defined which we want to be visible to Emacs. Each function must have an
# entry in it with the function as key (the function *object*, NOT its name as
# a string) and a string as value. At a minimum, this string will be empty,
# but it can contain the names of variables to be read interactively by the
# function, lisp-style.

# Functions meant to be used internally only (not exposed to Emacs) don't need
# an entry in interactions.

interactions = {}

#***************************************************************************
# WARNING: things from genutils copied verbatim here. For some reason pymacs
# does not import other modules correctly (my own, it seems ok with system
# stuff).

def indent(str,nspaces=4,ntabs=0):
    """Indent a string a given number of spaces or tabstops.

    indent(str,nspaces=4,ntabs=0) -> indent str by ntabs+nspaces.
    """

    ind = '\t'*ntabs+' '*nspaces
    outstr = '%s%s' % (ind,str.replace('\n','\n'+ind))
    if outstr.endswith('\n'+ind):
        return outstr[:-len(ind)]
    else:
        return outstr

# End of genutils copy/paste job.

#***************************************************************************
# Lisp utility functions, snatched from elsewhere.

def clean_undo_after(checkpoint):
        """\
Remove all intermediate boundaries from the Undo list since CHECKPOINT.
"""
        emacs("""
(let ((undo-list %s))
  (if (not (eq buffer-undo-list undo-list))
      (let ((cursor buffer-undo-list))
        (while (not (eq (cdr cursor) undo-list))
          (if (car (cdr cursor))
              (setq cursor (cdr cursor))
            (setcdr cursor (cdr (cdr cursor)))))))
  nil)
"""
             % (checkpoint or 'nil'))

#***************************************************************************
# Utility functions, none of which need an interactions[] entry.

def lisp_obj_info(obj):
    """Return various details about a lisp object as a string.

    Useful mainly for debugging purposes."""

    info = [obj,obj.__class__,obj.index,type(obj.index),repr(obj)]
    info = map(str,info)
    info = '\n'.join(info)
    return info

#---------------------------------------------------------------------------
def lisp_char(lisp_obj):
    """Return a single character string from a lisp char object.

    Used to extract characters from their lisp form as obtained in interactive
    functions with the c code. """
    text_form = repr(lisp_obj)
    try:
        return re.search(r"'\?(.)'",text_form).group(1)
    except:
        return None

#---------------------------------------------------------------------------
def is_yes(lisp_obj):
    """Check whether an interactive lisp character reply is a yes (y/Y)"""

    try:
        return lisp_char(lisp_obj).lower() == 'y'
    except:
        return 0

#---------------------------------------------------------------------------
def cut_region(mode='string'):
    """Return the active region and remove it from Emacs.

    The mode parameter (default 'string') defines whether to return the region
    as a string or as a list of lines (mode='list').

    It is the caller's responsibility to insert the updated text at the
    end back in the Emacs buffer with a call to emacs.insert(...)."""

    start, end = emacs.point(), emacs.mark(emacs.t)
    # BUG: buffer_substring() can't extract regions with dos line endings (\r\n)
    # It dumps a traceback.
    region = emacs.buffer_substring(start, end)
    if mode == 'list':
        region = region.splitlines()
    emacs.delete_region(start, end)
    return region
    # cut_region() doesn't need an entry in interactions[] b/c it's meant to
    # be used internally by other functions in this module, not directly
    # from Emacs

#---------------------------------------------------------------------------
def insert_text(text,offset=0):
    """Insert text in buffer and move cursor to a certain offset.

    If called with no offset, leaves the cursor at the current position."""

    # save undo state so we can roll everything into a single operation for undo
    checkpoint = emacs.buffer_undo_list.value()
    user_pos = emacs.point()
    emacs.insert(text)
    emacs.goto_char(user_pos+offset)
    # Collapse all operations into a single one, for Undo.
    clean_undo_after(checkpoint)

#---------------------------------------------------------------------------
def insert_indented_text(text,offset):
    """Insert indented text in buffer and move cursor to a certain offset."""

    # save undo state so we can roll everything into a single operation for undo
    checkpoint = emacs.buffer_undo_list.value()
    # figure out if we are indented or not, and adapt text accordingly
    indent_level = get_line_offset()
    if indent_level > 0:
        text = indent(text,indent_level)
    # perform actual insertion with proper cursor positioning
    offset += indent_level
    emacs.beginning_of_line()
    user_pos = emacs.point()
    emacs.insert(text)
    emacs.goto_char(user_pos+offset)
    # Collapse all operations into a single one, for Undo.
    clean_undo_after(checkpoint)

#---------------------------------------------------------------------------
def get_line_offset():
    """Return number of characters cursor is offset from margin.    """
    user_pos = emacs.point()
    emacs.beginning_of_line()
    line_start = emacs.point()
    emacs.goto_char(user_pos)
    return user_pos - line_start
# end get_line_offset()

#---------------------------------------------------------------------------
def newfn_string(name,sep,end,args=''):
    """Template for a new function definition.

    Returns the string containing the definition and the integer offset for
    cursor positioning."""

    # prepare text
    out = ''
    sep = lisp_char(sep)
    if sep is not None:
        out += '#'+sep*77+'\n'
    out += 'def '+name+'('+args
    offset = len(out)
    out += '):\n'
    out += '    """\n'*2
    if is_yes(end):
        out += '# end '+name+'()\n'
    return out,offset

#***************************************************************************
# 'Public' functions (exposed to Emacs). All these MUST have an interactions[]
# entry

def bow():
    """Break a region replacing all whitespace with newlines.

    Originally an example in Pymacs' README."""

    region = cut_region()
    emacs.insert('\n'.join(region.split()))

# Update interactions[] for functions meant to be visible in Emacs.

# Interaction strings follow some funny emacs-lisp conventions, with the first
# letter being a code and the rest a prompt. Use `C-h f interactive' in Emacs
# to get a description.  The simplest one is a prompt for a string, which is
# given as a string of the form 's<prompt>'.

# Will print 'name ' in the minibuffer and get a string:
#interactions[deft] = 'sNew function name? '

# The c code is for characters, and the Pymacs readme says they are returned
# to python as ints, but that doesn't seem to be the case. Instead I'm getting
# Pymacs.Lisp objects, which have a repr() of the form "lisp('?<char>')" where
# <char> is the returned character.

interactions[bow] = ''

# Note that trying to set interactions as a function attribute:
# bow.interactions = ''
# is NOT WORKING. The module loads in Emacs, but no functions are actually
# recognized. Tested with Python 2.1, it might work with Python 2.2

#-----------------------------------------------------------------------------
def dos2unix():
    """Remove DOS line endings from a region.
    """
    # Save undo state so we can roll everything into a single operation for undo
    checkpoint = emacs.buffer_undo_list.value()
    region = cut_region('list')
    emacs.insert('\n'.join(region)+'\n')
    # Collapse all operations into a single one, for Undo.
    clean_undo_after(checkpoint)

# BUG: it's not working b/c of a bug in emacs.buffer_substring(), so let's not
# activate it for now.
#interactions[dos2unix] = ''

#---------------------------------------------------------------------------
def newfn(name,sep,end,args=''):
    """Insert a template for a new function definition."""

    insert_indented_text(*newfn_string(name,sep,end))

new_template = 'sNew %s name? \n'\
               'cEnter separator (RET for none): \n'\
               'cPrint end marker (y/[N])? '

interactions[newfn] = new_template % 'function'

#-----------------------------------------------------------------------------
def newweave(name,sep,end,use_blitz):
    """Insert a template for a new weave function definition.
    """

    blitz,ending = '',''
    if is_yes(use_blitz):
        blitz = ",type_factories = blitz_type_factories"
    if is_yes(end):
        ending = "\n# end %s()" % (name,)

    head,offset = newfn_string(name,sep,0)
    head += \
'''
    code = \\
"""

"""
    return weave.inline(code,[]%(blitz)s)%(ending)s
''' % locals()
    insert_indented_text(head,offset)

interactions[newweave] = new_template % 'weave function'
interactions[newweave] += '\ncUse blitz type factories (y/[N])? '

#---------------------------------------------------------------------------
def newmeth(name,sep,end):
    """Insert a template for a new method definition.    """

    insert_indented_text(*newfn_string(name,sep,end,'self'))

interactions[newmeth] = new_template % 'method'

#---------------------------------------------------------------------------
def newclass(name,sep,end):
    """Template for new class definition.    """
    out =  ('class %s:\n' % (name,)) + ('    """\n'*2) + '\n'
    offset = get_line_offset()+len(out) + len ("    def __init__(self")
    new_str = newfn_string('__init__',None,None,'self')[0]
    out += indent(new_str)
    if is_yes(end):
        out += '# end class '+name+'\n'
    insert_indented_text(out,offset)

interactions[newclass] = new_template % 'class'
