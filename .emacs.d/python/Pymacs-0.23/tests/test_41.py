# -*- coding: utf-8 -*-

# Checking if the whole Pymacs works together.

import re
import setup
from Pymacs import lisp, pymacs

def setup_module(module):
    setup.start_emacs()

def teardown_module(module):
    setup.stop_emacs()

def test_1():

    def validate(input, expected):
        output = re.sub(r'\(pymacs-(defun|python) \. [0-9]*\)',
                        r'(pymacs-\1 . 0)',
                        setup.ask_emacs('(pymacs-eval %s)' % input, 'prin1'))
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
        pymacs.print_lisp(repr(input), fragments.append, quotable)
        yield validate, ''.join(fragments), output
    for input, output in (
            ('ord', ('(lambda (&rest arguments)'
                     ' (pymacs-apply (quote (pymacs-python . 0)) arguments))')),
            ('object()', '(pymacs-python . 0)'),
            ):
        fragments = []
        pymacs.print_lisp(input, fragments.append, True)
        yield validate, '\'' + ''.join(fragments), output

def test_2():

    def validate(input, expected):
        output = setup.ask_emacs(input, 'prin1')
        assert output == expected, (output, expected)

    yield validate, '(pymacs-eval "3 + 5")', '8'

    yield (validate,
           ('(progn (pymacs-exec "def f(): pass")\n'
            '       (pymacs-eval "lisp.apply(f, None)"))\n'),
           'nil')

    yield (validate,
           '(pymacs-eval "lisp(\'(pymacs-eval \\"repr(2L**111)\\")\')")',
           '"2596148429267413814265248164610048L"')

def test_3():

    setup.ask_emacs('(pymacs-exec "import os\nimport sys")')

def test_4():
    # Try ``list.buffer_string()`` in a multi-byte buffer.
    pass
