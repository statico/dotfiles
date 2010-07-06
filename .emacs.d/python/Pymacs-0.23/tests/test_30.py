# -*- coding: utf-8 -*-

# Checking if pymacs.el loads.

import setup

def test_1():
    setup.start_emacs()
    setup.stop_emacs()
