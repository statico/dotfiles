# -*- coding: utf-8 -*-

# Checking if Emacs loads the Python helper.

import setup

def test_1():
    setup.start_emacs()
    output = setup.ask_emacs(('(progn\n'
                              '  (pymacs-start-services)\n'
                              '  (not (null pymacs-transit-buffer)))\n'),
                             'prin1')
    assert output == 't', output
    setup.stop_emacs()
