# -*- coding: utf-8 -*-

# Checking if pymacs.py works (Emacs and the Pymacs helper are not used).

import re
import setup
from Pymacs import lisp, pymacs

def test_print_lisp():

    def validate(input, quoted, expected):
        fragments = []
        pymacs.print_lisp(input, fragments.append, quoted)
        output = re.sub(r'\(pymacs-(defun|python) [0-9]*',
                        r'(pymacs-\1 0',
                        ''.join(fragments))
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
            (False, 'f\bz', r'"f\bz"'),
            (False, 'g\fz', r'"g\fz"'),
            (False, 'h\nz', r'"h\nz"'),
            (False, 'i\rz', r'"i\rz"'),
            (False, 'j\r\nz', r'"j\r\nz"'),
            (False, 'k\tz', r'"k\tz"'),
            (False, 'l\x1bz', r'"l\033z"'),
            (False, u'p', '"p"'),
            (False, u'qyz', '"qyz"'),
            (False, u'rêvé', (r'(decode-coding-string "r\303\252v\303\251"'
                              ' \'utf-8)')),
            (False, u's—z!', (r'(decode-coding-string "s\342\200\224z!"'
                              ' \'utf-8)')),
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
            (False, ord, '(pymacs-defun 0 nil)'),
            (False, object(), '(pymacs-python 0)'),
            ):
        yield validate, input, False, output
        if quotable:
            yield validate, input, True, '\'' + output
        else:
            yield validate, input, True, output
