# -*- coding: utf-8 -*-

# Checking if pymacs.el works (the Pymacs helper is not used).

import re
import setup
from Pymacs import lisp, pymacs

def setup_module(module):
    setup.start_emacs()
    setup.ask_emacs('(defun print-for-eval-expanded (expression)\n'
                    '  (let ((pymacs-forget-mutability t))\n'
                    '    (pymacs-print-for-eval expression)))\n')

def teardown_module(module):
    setup.stop_emacs()

def test_1():

    def validate(input, expected):
        output = setup.ask_emacs(input, 'prin1')
        output = re.sub(r'\(pymacs-(defun|python) [0-9]*',
                        r'(pymacs-\1 0',
                        output)
        assert output == expected, (output, expected)

    for quotable, input, output in (
            (False, None, 'nil'),
            (False, False, 'nil'),
            (False, True, 't'),
            (False, 3, '3'),
            (False, 0, '0'),
            (False, -3, '-3'),
            (False, 3., '3.0'),
            (False, 0., '0.0'),
            (False, -3., '-3.0'),
            (False, '', '""'),
            (False, 'a', '"a"'),
            (False, 'byz', '"byz"'),
            (False, 'c\'bz', '"c\'bz"'),
            (False, 'd"z', r'"d\"z"'),
            (False, 'e\\bz', r'"e\\bz"'),
            (False, 'f\bz', '"f\bz"'),
            (False, 'g\fz', '"g\fz"'),
            (False, 'h\nz', '"h\nz"'),
            (False, 'i\rz', '"i\rz"'),
            (False, 'j\r\nz', '"j\r\nz"'),
            (False, 'k\tz', '"k\tz"'),
            (False, 'l\x1bz', '"l\x1bz"'),
            (False, u'p', '"p"'),
            (False, u'qyz', '"qyz"'),
            (False, u'rêvé', '"r\303\252v\303\251"'),
            (False, u's—z!', '"s\342\200\224z!"'),
            (False, (), '[]'),
            (False, (0,), '[0]'),
            (False, (0.0,), '[0.0]'),
            (False, ('a',), '["a"]'),
            (False, (0, 0.0, "a"), '[0 0.0 "a"]'),
            (True, [], 'nil'),
            (True, [0], '(0)'),
            (True, [0.0], '(0.0)'),
            (True, ['a'], '("a")'),
            (True, [0, 0.0, "a"], '(0 0.0 "a")'),
            (False, lisp['nil'], 'nil'),
            (True, lisp['t'], 't'),
            (True, lisp['ab_cd'], 'ab_cd'),
            (True, lisp['ab-cd'], 'ab-cd'),
            (True, lisp['lambda'], 'lambda'),
            (False, lisp.nil, 'nil'),
            (True, lisp.t, 't'),
            (True, lisp.ab_cd, 'ab-cd'),
            # TODO: Lisp and derivatives
            ):
        fragments = []
        pymacs.print_lisp(input, fragments.append, quotable)
        yield validate, ''.join(fragments), output
    for input, output in (
            (ord, '(pymacs-defun 0 nil)'),
            (object(), '(pymacs-python 0)'),
            ):
        fragments = []
        pymacs.print_lisp(input, fragments.append, True)
        yield validate, '\'' + ''.join(fragments), output

def test_2():

    def validate(input, expected):
        output = setup.ask_emacs(input, 'print-for-eval-expanded')
        output = re.sub(r'\(pymacs-(defun|python) [0-9]*',
                        r'(pymacs-\1 0',
                        output)
        assert output == expected, (output, expected)

    for quotable, input, output in (
            (False, None, 'None'),
            (False, False, 'None'),
            (False, True, 'True'),
            (False, 3, '3'),
            (False, 0, '0'),
            (False, -3, '-3'),
            (False, 3., '3.0'),
            (False, 0., '0.0'),
            (False, -3., '-3.0'),
            (False, '', '""'),
            (False, 'a', '"a"'),
            (False, 'byz', '"byz"'),
            (False, 'c\'bz', '"c\'bz"'),
            (False, 'd"z', r'"d\"z"'),
            (False, 'e\\bz', r'"e\\bz"'),
            (False, 'f\bz', '"f\x08z"'),
            (False, 'g\fz', '"g\x0cz"'),
            (False, 'h\nz', r'"h\nz"'),
            (False, 'i\rz', '"i\rz"'),
            (False, 'j\r\nz', '"j\r\\nz"'),
            (False, 'k\tz', '"k\tz"'),
            (False, 'l\x1bz', '"l\x1bz"'),
            (False, (), '()'),
            (False, (0,), '(0,)'),
            (False, (0.0,), '(0.0,)'),
            (False, ('a',), '("a",)'),
            (False, (0, 0.0, "a"), '(0, 0.0, "a")'),
            (True, [], 'None'),
            (True, [0], '[0]'),
            (True, [0.0], '[0.0]'),
            (True, ['a'], '["a"]'),
            (True, [0, 0.0, "a"], '[0, 0.0, "a"]'),
            (False, lisp['nil'], 'None'),
            (True, lisp['t'], 'True'),
            (True, lisp['ab_cd'], 'lisp["ab_cd"]'),
            (True, lisp['ab-cd'], 'lisp["ab-cd"]'),
            (True, lisp['lambda'], 'lisp["lambda"]'),
            (False, lisp.nil, 'None'),
            (True, lisp.t, 'True'),
            (True, lisp.ab_cd, 'lisp["ab-cd"]'),
            # TODO: Lisp and derivatives
            ):
        fragments = []
        pymacs.print_lisp(input, fragments.append, quotable)
        yield validate, ''.join(fragments), output
    #for input, output in (
    #        (ord, '(pymacs-defun 0 nil)'),
    #        (object(), '(pymacs-python 0)'),
    #        ):
    #    fragments = []
    #    pymacs.print_lisp(input, fragments.append, True)
    #    yield validate, '\'' + ''.join(fragments), output
