#!/usr/bin/env python
# -*- coding: utf-8 -*-
# Copyright © 1991-1998, 2000, 2002, 2003 Progiciels Bourbeau-Pinard inc.
# François Pinard <pinard@iro.umontreal.ca>, 1991-04.

# This program is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 2, or (at your option)
# any later version.

# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.

# You should have received a copy of the GNU General Public License
# along with this program; if not, write to the Free Software Foundation,
# Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.

"""\
Handling of boxed comments in various box styles.

The user documentation for this tool may be found at:

    http://pymacs.progiciels-bpi.ca/rebox.html
"""

## Note: a double hash comment introduces a group of functions or methods.

__metatype__ = type
import re, sys

## Batch specific features.

def main(*arguments):
    refill = True
    style = None
    tabify = False
    verbose = False
    width = 79
    import getopt
    options, arguments = getopt.getopt(arguments, 'ns:tvw:', ['help'])
    for option, value in options:
        if option == '--help':
            sys.stdout.write(__doc__)
            sys.exit(0)
        elif option == '-n':
            refill = False
        elif option == '-s':
            style = int(value)
        elif option == '-t':
            tabify = True
        elif option == '-v':
            verbose = True
        elif option == '-w':
            width = int(value)
    if len(arguments) == 0:
        text = sys.stdin.read()
    elif len(arguments) == 1:
        text = open(arguments[0]).read()
    else:
        sys.stderr.write("Invalid usage, try `rebox --help' for help.\n")
        sys.exit(1)
    old_style, new_style, text, position = engine(
        text, style=style, width=width, refill=refill, tabify=tabify)
    if text is None:
        sys.stderr.write("* Cannot rebox to style %d.\n" % new_style)
        sys.exit(1)
    sys.stdout.write(text)
    if verbose:
        if old_style == new_style:
            sys.stderr.write("Reboxed with style %d.\n" % old_style)
        else:
            sys.stderr.write("Reboxed from style %d to %d.\n"
                             % (old_style, new_style))

## Emacs specific features.

def pymacs_load_hook():
    global interactions, lisp, Let, region, comment, set_default_style
    from Pymacs import lisp, Let
    emacs_rebox = Emacs_Rebox()
    # Declare functions for Emacs to import.
    interactions = {}
    region = emacs_rebox.region
    interactions[region] = 'P'
    comment = emacs_rebox.comment
    interactions[comment] = 'P'
    set_default_style = emacs_rebox.set_default_style

class Emacs_Rebox:

    def __init__(self):
        self.default_style = None

    def set_default_style(self, style):
        """\
Set the default style to STYLE.
"""
        self.default_style = style

    def region(self, flag):
        """\
Rebox the boxed comment in the current region, obeying FLAG.
"""
        self.emacs_engine(flag, self.find_region)

    def comment(self, flag):
        """\
Rebox the surrounding boxed comment, obeying FLAG.
"""
        self.emacs_engine(flag, self.find_comment)

    def emacs_engine(self, flag, find_limits):
        """\
Rebox text while obeying FLAG.  Call FIND_LIMITS to discover the extent
of the boxed comment.
"""
        # `C-u -' means that box style is to be decided interactively.
        if flag == lisp['-']:
            flag = self.ask_for_style()
        # If FLAG is zero or negative, only change default box style.
        if isinstance(flag, int) and flag <= 0:
            self.default_style = -flag
            lisp.message("Default style set to %d" % -flag)
            return
        # Decide box style and refilling.
        if flag is None:
            style = self.default_style
            refill = True
        elif isinstance(flag, int):
            if self.default_style is None:
                style = flag
            else:
                style = merge_styles(self.default_style, flag)
            refill = True
        else:
            flag = flag.copy()
            if isinstance(flag, list):
                style = self.default_style
                refill = False
            else:
                lisp.error("Unexpected flag value %s" % flag)
        # Prepare for reboxing.
        lisp.message("Reboxing...")
        checkpoint = lisp.buffer_undo_list.value()
        start, end = find_limits()
        text = lisp.buffer_substring(start, end)
        width = lisp.fill_column.value()
        tabify = lisp.indent_tabs_mode.value() is not None
        point = lisp.point()
        if start <= point < end:
            position = point - start
        else:
            position = None
        # Rebox the text and replace it in Emacs buffer.
        old_style, new_style, text, position = engine(
            text, style=style, width=width,
            refill=refill, tabify=tabify, position=position)
        if text is None:
            lisp.error("Cannot rebox to style %d" % new_style)
        lisp.delete_region(start, end)
        lisp.insert(text)
        if position is not None:
            lisp.goto_char(start + position)
        # Collapse all operations into a single one, for Undo.
        self.clean_undo_after(checkpoint)
        # We are finished, tell the user.
        if old_style == new_style:
            lisp.message("Reboxed with style %d" % old_style)
        else:
            lisp.message("Reboxed from style %d to %d"
                         % (old_style, new_style))

    def ask_for_style(self):
        """\
Request the style interactively, using the minibuffer.
"""
        language = quality = type = None
        while language is None:
            lisp.message("\
Box language is 100-none, 200-/*, 300-//, 400-#, 500-;, 600-%%")
            key = lisp.read_char()
            if key >= ord('0') and key <= ord('6'):
                language = key - ord('0')
        while quality is None:
            lisp.message("\
Box quality/width is 10-simple/1, 20-rounded/2, 30-starred/3 or 40-starred/4")
            key = lisp.read_char()
            if key >= ord('0') and key <= ord('4'):
                quality = key - ord('0')
        while type is None:
            lisp.message("\
Box type is 1-opened, 2-half-single, 3-single, 4-half-double or 5-double")
            key = lisp.read_char()
            if key >= ord('0') and key <= ord('5'):
                type = key - ord('0')
        return 100*language + 10*quality + type

    def find_region(self):
        """\
Return the limits of the region.
"""
        return lisp.point(), lisp.mark(lisp.t)

    def find_comment(self):
        """\
Find and return the limits of the block of comments following or enclosing
the cursor, or return an error if the cursor is not within such a block
of comments.  Extend it as far as possible in both directions.
"""
        let = Let()
        let.push_excursion()
        # Find the start of the current or immediately following comment.
        lisp.beginning_of_line()
        lisp.skip_chars_forward(' \t\n')
        lisp.beginning_of_line()
        if not language_matcher[0](self.remainder_of_line()):
            temp = lisp.point()
            if not lisp.re_search_forward('\\*/', None, lisp.t):
                lisp.error("outside any comment block")
            lisp.re_search_backward('/\\*')
            if lisp.point() > temp:
                lisp.error("outside any comment block")
            temp = lisp.point()
            lisp.beginning_of_line()
            lisp.skip_chars_forward(' \t')
            if lisp.point() != temp:
                lisp.error("text before start of comment")
            lisp.beginning_of_line()
        start = lisp.point()
        language = guess_language(self.remainder_of_line())
        # Find the end of this comment.
        if language == 2:
            lisp.search_forward('*/')
            if not lisp.looking_at('[ \t]*$'):
                lisp.error("text after end of comment")
        lisp.end_of_line()
        if lisp.eobp():
            lisp.insert('\n')
        else:
            lisp.forward_char(1)
        end = lisp.point()
        # Try to extend the comment block backwards.
        lisp.goto_char(start)
        while not lisp.bobp():
            if language == 2:
                lisp.skip_chars_backward(' \t\n')
                if not lisp.looking_at('[ \t]*\n[ \t]*/\\*'):
                    break
                if lisp.point() < 2:
                    break
                lisp.backward_char(2)
                if not lisp.looking_at('\\*/'):
                    break
                lisp.re_search_backward('/\\*')
                temp = lisp.point()
                lisp.beginning_of_line()
                lisp.skip_chars_forward(' \t')
                if lisp.point() != temp:
                    break
                lisp.beginning_of_line()
            else:
                lisp.previous_line(1)
                if not language_matcher[language](self.remainder_of_line()):
                    break
            start = lisp.point()
        # Try to extend the comment block forward.
        lisp.goto_char(end)
        while language_matcher[language](self.remainder_of_line()):
            if language == 2:
                lisp.re_search_forward('[ \t]*/\\*')
                lisp.re_search_forward('\\*/')
                if lisp.looking_at('[ \t]*$'):
                    lisp.beginning_of_line()
                    lisp.forward_line(1)
                    end = lisp.point()
            else:
                lisp.forward_line(1)
                end = lisp.point()
        return start, end

    def remainder_of_line(self):
        """\
Return all characters between point and end of line in Emacs buffer.
"""
        return lisp('''\
(buffer-substring (point) (save-excursion (skip-chars-forward "^\n") (point)))
''')

    def clean_undo_after_old(self, checkpoint):
        """\
Remove all intermediate boundaries from the Undo list since CHECKPOINT.
"""
        # Declare some Lisp functions.
        car = lisp.car
        cdr = lisp.cdr
        eq = lisp.eq
        setcdr = lisp.setcdr
        # Remove any `nil' delimiter recently added to the Undo list.
        cursor = lisp.buffer_undo_list.value()
        if not eq(cursor, checkpoint):
            tail = cdr(cursor)
            while not eq(tail, checkpoint):
                if car(tail):
                    cursor = tail
                    tail = cdr(cursor)
                else:
                    tail = cdr(tail)
                    setcdr(cursor, tail)

    def clean_undo_after(self, checkpoint):
        """\
Remove all intermediate boundaries from the Undo list since CHECKPOINT.
"""
        lisp("""
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

## Reboxing main control.

def engine(text, style=None, width=79, refill=True, tabify=False,
           position=None):
    """\
Add, delete or adjust a boxed comment held in TEXT, according to STYLE.
STYLE values are explained at beginning of this file.  Any zero attribute
in STYLE indicates that the corresponding attribute should be recovered
from the currently existing box.  Produced lines will not go over WIDTH
columns if possible, if refilling gets done.  But if REFILL is false, WIDTH
is ignored.  If TABIFY is true, the beginning of produced lines will have
spaces replace by TABs.  POSITION is either None, or a character position
within TEXT.  Returns four values: the old box style, the new box style,
the reformatted text, and either None or the adjusted value of POSITION in
the new text.  The reformatted text is returned as None if the requested
style does not exist.
"""
    last_line_complete = text and text[-1] == '\n'
    if last_line_complete:
        text = text[:-1]
    lines = text.expandtabs().split('\n')
    # Decide about refilling and the box style to use.
    new_style = 111
    old_template = guess_template(lines)
    new_style = merge_styles(new_style, old_template.style)
    if style is not None:
        new_style = merge_styles(new_style, style)
    new_template = template_registry.get(new_style)
    # Interrupt processing if STYLE does not exist.
    if not new_template:
        return old_template.style, new_style, None, None
    # Remove all previous comment marks, and left margin.
    if position is not None:
        marker = Marker()
        marker.save_position(text, position, old_template.characters())
    lines, margin = old_template.unbuild(lines)
    # Ensure only one white line between paragraphs.
    counter = 1
    while counter < len(lines) - 1:
        if lines[counter] == '' and lines[counter-1] == '':
            del lines[counter]
        else:
            counter = counter + 1
    # Rebuild the boxed comment.
    lines = new_template.build(lines, width, refill, margin)
    # Retabify to the left only.
    if tabify:
        for counter in range(len(lines)):
            tabs = len(re.match(' *', lines[counter]).group()) / 8
            lines[counter] = '\t' * tabs + lines[counter][8*tabs:]
    # Restore the point position.
    text = '\n'.join(lines)
    if last_line_complete:
        text = text + '\n'
    if position is not None:
        position = marker.get_position(text, new_template.characters())
    return old_template.style, new_style, text, position

def guess_language(line):
    """\
Guess the language in use for LINE.
"""
    for language in range(len(language_matcher) - 1, 1, -1):
        if language_matcher[language](line):
            return language
    return 1

def guess_template(lines):
    """\
Find the heaviest box template matching LINES.
"""
    best_template = None
    for template in template_registry.values():
        if best_template is None or template > best_template:
            if template.match(lines):
                best_template = template
    return best_template

def left_margin_size(lines):
    """\
Return the width of the left margin for all LINES.  Ignore white lines.
"""
    margin = None
    for line in lines:
        counter = len(re.match(' *', line).group())
        if counter != len(line):
            if margin is None or counter < margin:
                margin = counter
    if margin is None:
        margin = 0
    return margin

def merge_styles(original, update):
    """\
Return style attributes as per ORIGINAL, in which attributes have been
overridden by non-zero corresponding style attributes from UPDATE.
"""
    style = [original / 100, original / 10 % 10, original % 10]
    merge = update / 100, update / 10 % 10, update % 10
    for counter in range(3):
        if merge[counter]:
            style[counter] = merge[counter]
    return 100*style[0] + 10*style[1] + style[2]

## Refilling logic.

def refill_lines(lines, width,
                 cached_refiller=[]):
    """\
Refill LINES, trying to not produce lines having more than WIDTH columns.
"""
    if not cached_refiller:
        for Refiller in Refiller_Gnu_Fmt, Refiller_Textwrap, Refiller_Dumb:
            refiller = Refiller()
            new_lines = refiller.fill(lines, width)
            if new_lines is not None:
                cached_refiller.append(refiller)
                return new_lines
    return cached_refiller[0].fill(lines, width)

class Refiller:
    available = True

    def fill(self, lines, width):
        if self.available:
            new_lines = []
            start = 0
            while start < len(lines) and not lines[start]:
                start = start + 1
            end = start
            while end < len(lines):
                while end < len(lines) and lines[end]:
                    end = end + 1
                new_lines = new_lines + self.fill_paragraph(lines[start:end],
                                                            width)
                while end < len(lines) and not lines[end]:
                    end = end + 1
                if end < len(lines):
                    new_lines.append('')
                    start = end
            return new_lines

class Refiller_Gnu_Fmt(Refiller):
    """\
Use both Knuth algorithm and protection for full stops at end of sentences.
"""

    def fill(self, lines, width):
        if self.available:
            import tempfile, os
            name = tempfile.mktemp()
            open(name, 'w').write('\n'.join(lines) + '\n')
            process = os.popen('fmt -cuw %d %s' % (width, name))
            text = process.read()
            os.remove(name)
            if process.close() is None:
                return [line.expandtabs() for line in text.split('\n')[:-1]]

class Refiller_Textwrap(Refiller):
    """\
No Knuth algorithm, but protection for full stops at end of sentences.
"""
    def __init__(self):
        try:
            from textwrap import TextWrapper
        except ImportError:
            self.available = False
        else:
            self.wrapper = TextWrapper(fix_sentence_endings=1)

    def fill_paragraph(self, lines, width):
        # FIXME: This one fills indented lines more aggressively than the
        # dumb refiller.  I'm not sure what it the best thing to do, but
        # ideally, all refillers should behave more or less the same way.
        self.wrapper.width = width
        prefix = ' ' * left_margin_size(lines)
        self.wrapper.initial_indent = prefix
        self.wrapper.subsequent_indent = prefix
        return self.wrapper.wrap(' '.join(lines))

class Refiller_Dumb(Refiller):
    """\
No Knuth algorithm, nor even protection for full stops at end of sentences.
"""

    def fill_paragraph(self, lines, width):
        margin = left_margin_size(lines)
        prefix = ' ' * margin
        new_lines = []
        new_line = ''
        for line in lines:
            counter = len(line) - len(line.lstrip())
            if counter > margin:
                if new_line:
                    new_lines.append(prefix + new_line)
                    new_line = ''
                indent = ' ' * (counter - margin)
            else:
                indent = ''
            for word in line.split():
                if new_line:
                    if len(new_line) + 1 + len(word) > width:
                        new_lines.append(prefix + new_line)
                        new_line = word
                    else:
                        new_line = new_line + ' ' + word
                else:
                    new_line = indent + word
                    indent = ''
        if new_line:
            new_lines.append(prefix + new_line)
        return new_lines

## Marking logic.

class Marker:
    """\
Heuristics to simulate a marker while reformatting boxes.
"""

    def save_position(self, text, position, ignorable):
        """\
Given a TEXT and a POSITION in that text, save the adjusted position
by faking that all IGNORABLE characters before POSITION were removed.
"""
        ignore = {}
        for character in ' \t\r\n' + ignorable:
            ignore[character] = None
        counter = 0
        for character in text[:position]:
            if ignore.has_key(character):
                counter = counter + 1
        self.position = position - counter

    def get_position(self, text, ignorable, latest=0):
        """\
Given a TEXT, return the value that would yield the currently saved position,
if it was saved by `save_position' with IGNORABLE.  Unless the position lies
within a series of ignorable characters, LATEST has no effect in practice.
If LATEST is true, return the biggest possible value instead of the smallest.
"""
        ignore = {}
        for character in ' \t\r\n' + ignorable:
            ignore[character] = None
        counter = 0
        position = 0
        if latest:
            for character in text:
                if ignore.has_key(character):
                    counter = counter + 1
                else:
                    if position == self.position:
                        break
                    position = position + 1
        elif self.position > 0:
            for character in text:
                if ignore.has_key(character):
                    counter = counter + 1
                else:
                    position = position + 1
                    if position == self.position:
                        break
        return position + counter

## Template processing.

class Template:

    def __init__(self, style, weight, lines):
        """\
Digest and register a single template.  The template is numbered STYLE,
has a parsing WEIGHT, and is described by one to three LINES.
STYLE should be used only once through all `declare_template' calls.

One of the lines should contain the substring `box' to represent the comment
to be boxed, and if three lines are given, `box' should appear in the middle
one.  Lines containing only spaces are implied as necessary before and after
the the `box' line, so we have three lines.

Normally, all three template lines should be of the same length.  If the first
line is shorter, it represents a start comment string to be bundled within the
first line of the comment text.  If the third line is shorter, it represents
an end comment string to be bundled at the end of the comment text, and
refilled with it.
"""
        assert not template_registry.has_key(style), \
               "Style %d defined more than once" % style
        self.style = style
        self.weight = weight
        # Make it exactly three lines, with `box' in the middle.
        start = lines[0].find('box')
        if start >= 0:
            line1 = None
            line2 = lines[0]
            if len(lines) > 1:
                line3 = lines[1]
            else:
                line3 = None
        else:
            start = lines[1].find('box')
            if start >= 0:
                line1 = lines[0]
                line2 = lines[1]
                if len(lines) > 2:
                    line3 = lines[2]
                else:
                    line3 = None
            else:
                assert 0, "Erroneous template for %d style" % style
        end = start + len('box')
        # Define a few booleans.
        self.merge_nw = line1 is not None and len(line1) < len(line2)
        self.merge_se = line3 is not None and len(line3) < len(line2)
        # Define strings at various cardinal directions.
        if line1 is None:
            self.nw = self.nn = self.ne = None
        elif self.merge_nw:
            self.nw = line1
            self.nn = self.ne = None
        else:
            if start > 0:
                self.nw = line1[:start]
            else:
                self.nw = None
            if line1[start] != ' ':
                self.nn = line1[start]
            else:
                self.nn = None
            if end < len(line1):
                self.ne = line1[end:].rstrip()
            else:
                self.ne = None
        if start > 0:
            self.ww = line2[:start]
        else:
            self.ww = None
        if end < len(line2):
            self.ee = line2[end:]
        else:
            self.ee = None
        if line3 is None:
            self.sw = self.ss = self.se = None
        elif self.merge_se:
            self.sw = self.ss = None
            self.se = line3.rstrip()
        else:
            if start > 0:
                self.sw = line3[:start]
            else:
                self.sw = None
            if line3[start] != ' ':
                self.ss = line3[start]
            else:
                self.ss = None
            if end < len(line3):
                self.se = line3[end:].rstrip()
            else:
                self.se = None
        # Define parsing regexps.
        if self.merge_nw:
            self.regexp1 = re.compile(' *' + regexp_quote(self.nw) + '.*$')
        elif self.nw and not self.nn and not self.ne:
            self.regexp1 = re.compile(' *' + regexp_quote(self.nw) + '$')
        elif self.nw or self.nn or self.ne:
            self.regexp1 = re.compile(
                ' *' + regexp_quote(self.nw) + regexp_ruler(self.nn)
                + regexp_quote(self.ne) + '$')
        else:
            self.regexp1 = None
        if self.ww or self.ee:
            self.regexp2 = re.compile(
                ' *' + regexp_quote(self.ww) + '.*'
                + regexp_quote(self.ee) + '$')
        else:
            self.regexp2 = None
        if self.merge_se:
            self.regexp3 = re.compile('.*' + regexp_quote(self.se) + '$')
        elif self.sw and not self.ss and not self.se:
            self.regexp3 = re.compile(' *' + regexp_quote(self.sw) + '$')
        elif self.sw or self.ss or self.se:
            self.regexp3 = re.compile(
                ' *' + regexp_quote(self.sw) + regexp_ruler(self.ss)
                + regexp_quote(self.se) + '$')
        else:
            self.regexp3 = None
        # Save results.
        template_registry[style] = self

    def __cmp__(self, other):
        return cmp(self.weight, other.weight)

    def characters(self):
        """\
Return a string of characters which may be used to draw the box.
"""
        characters = ''
        for text in (self.nw, self.nn, self.ne,
                     self.ww, self.ee,
                     self.sw, self.ss, self.se):
            if text:
                for character in text:
                    if character not in characters:
                        characters = characters + character
        return characters

    def match(self, lines):
        """\
Returns true if LINES exactly match this template.
"""
        start = 0
        end = len(lines)
        if self.regexp1 is not None:
            if start == end or not self.regexp1.match(lines[start]):
                return 0
            start = start + 1
        if self.regexp3 is not None:
            if end == 0 or not self.regexp3.match(lines[end-1]):
                return 0
            end = end - 1
        if self.regexp2 is not None:
            for line in lines[start:end]:
                if not self.regexp2.match(line):
                    return 0
        return 1

    def unbuild(self, lines):
        """\
Remove all comment marks from LINES, as hinted by this template.  Returns the
cleaned up set of lines, and the size of the left margin.
"""
        margin = left_margin_size(lines)
        # Remove box style marks.
        start = 0
        end = len(lines)
        if self.regexp1 is not None:
            lines[start] = unbuild_clean(lines[start], self.regexp1)
            start = start + 1
        if self.regexp3 is not None:
            lines[end-1] = unbuild_clean(lines[end-1], self.regexp3)
            end = end - 1
        if self.regexp2 is not None:
            for counter in range(start, end):
                lines[counter] = unbuild_clean(lines[counter], self.regexp2)
        # Remove the left side of the box after it turned into spaces.
        delta = left_margin_size(lines) - margin
        for counter in range(len(lines)):
            lines[counter] = lines[counter][delta:]
        # Remove leading and trailing white lines.
        start = 0
        end = len(lines)
        while start < end and lines[start] == '':
            start = start + 1
        while end > start and lines[end-1] == '':
            end = end - 1
        return lines[start:end], margin

    def build(self, lines, width, refill, margin):
        """\
Put LINES back into a boxed comment according to this template, after
having refilled them if REFILL.  The box should start at column MARGIN,
and the total size of each line should ideally not go over WIDTH.
"""
        # Merge a short end delimiter now, so it gets refilled with text.
        if self.merge_se:
            if lines:
                lines[-1] = lines[-1] + '  ' + self.se
            else:
                lines = [self.se]
        # Reduce WIDTH according to left and right inserts, then refill.
        if self.ww:
            width = width - len(self.ww)
        if self.ee:
            width = width - len(self.ee)
        if refill:
            lines = refill_lines(lines, width)
        # Reduce WIDTH further according to the current right margin,
        # and excluding the left margin.
        maximum = 0
        for line in lines:
            if line:
                if line[-1] in '.!?':
                    length = len(line) + 1
                else:
                    length = len(line)
                if length > maximum:
                    maximum = length
        width = maximum - margin
        # Construct the top line.
        if self.merge_nw:
            lines[0] = ' ' * margin + self.nw + lines[0][margin:]
            start = 1
        elif self.nw or self.nn or self.ne:
            if self.nn:
                line = self.nn * width
            else:
                line = ' ' * width
            if self.nw:
                line = self.nw + line
            if self.ne:
                line = line + self.ne
            lines.insert(0, (' ' * margin + line).rstrip())
            start = 1
        else:
            start = 0
        # Construct all middle lines.
        for counter in range(start, len(lines)):
            line = lines[counter][margin:]
            line = line + ' ' * (width - len(line))
            if self.ww:
                line = self.ww + line
            if self.ee:
                line = line + self.ee
            lines[counter] = (' ' * margin + line).rstrip()
        # Construct the bottom line.
        if self.sw or self.ss or self.se and not self.merge_se:
            if self.ss:
                line = self.ss * width
            else:
                line = ' ' * width
            if self.sw:
                line = self.sw + line
            if self.se and not self.merge_se:
                line = line + self.se
            lines.append((' ' * margin + line).rstrip())
        return lines

def regexp_quote(text):
    """\
Return a regexp matching TEXT without its surrounding space, maybe
followed by spaces.  If STRING is nil, return the empty regexp.
Unless spaces, the text is nested within a regexp parenthetical group.
"""
    if text is None:
        return ''
    if text == ' ' * len(text):
        return ' *'
    return '(' + re.escape(text.strip()) + ') *'

def regexp_ruler(character):
    """\
Return a regexp matching two or more repetitions of CHARACTER, maybe
followed by spaces.  Is CHARACTER is nil, return the empty regexp.
Unless spaces, the ruler is nested within a regexp parenthetical group.
"""
    if character is None:
        return ''
    if character == ' ':
        return '  +'
    return '(' + re.escape(character + character) + '+) *'

def unbuild_clean(line, regexp):
    """\
Return LINE with all parenthetical groups in REGEXP erased and replaced by an
equivalent number of spaces, except for trailing spaces, which get removed.
"""
    match = re.match(regexp, line)
    groups = match.groups()
    for counter in range(len(groups)):
        if groups[counter] is not None:
            start, end = match.span(1 + counter)
            line = line[:start] + ' ' * (end - start) + line[end:]
    return line.rstrip()

## Template data.

# Matcher functions for a comment start, indexed by numeric LANGUAGE.
language_matcher = []
for pattern in (r' *(/\*|//+|#+|;+|%+)',
                r'',            # 1
                r' */\*',       # 2
                r' *//+',       # 3
                r' *#+',        # 4
                r' *;+',        # 5
                r' *%+'):       # 6
    language_matcher.append(re.compile(pattern).match)

# Template objects, indexed by numeric style.
template_registry = {}

def make_generic(style, weight, lines):
    """\
Add various language digit to STYLE and generate one template per language,
all using the same WEIGHT.  Replace `?' in LINES accordingly.
"""
    for language, character in ((300, '/'),  # C++ style comments
                                (400, '#'),  # scripting languages
                                (500, ';'),  # Lisp and assembler
                                (600, '%')): # TeX and PostScript
        new_style = language + style
        if 310 < new_style <= 319:
            # Disallow quality 10 with C++.
            continue
        new_lines = []
        for line in lines:
            new_lines.append(line.replace('?', character))
        Template(new_style, weight, new_lines)

# Generic programming language templates.

make_generic(11, 115, ('? box',))

make_generic(12, 215, ('? box ?',
                       '? --- ?'))

make_generic(13, 315, ('? --- ?',
                       '? box ?',
                       '? --- ?'))

make_generic(14, 415, ('? box ?',
                       '???????'))

make_generic(15, 515, ('???????',
                       '? box ?',
                       '???????'))

make_generic(21, 125, ('?? box',))

make_generic(22, 225, ('?? box ??',
                       '?? --- ??'))

make_generic(23, 325, ('?? --- ??',
                       '?? box ??',
                       '?? --- ??'))

make_generic(24, 425, ('?? box ??',
                       '?????????'))

make_generic(25, 525, ('?????????',
                       '?? box ??',
                       '?????????'))

make_generic(31, 135, ('??? box',))

make_generic(32, 235, ('??? box ???',
                       '??? --- ???'))

make_generic(33, 335, ('??? --- ???',
                       '??? box ???',
                       '??? --- ???'))

make_generic(34, 435, ('??? box ???',
                       '???????????'))

make_generic(35, 535, ('???????????',
                       '??? box ???',
                       '???????????'))

make_generic(41, 145, ('???? box',))

make_generic(42, 245, ('???? box ????',
                       '???? --- ????'))

make_generic(43, 345, ('???? --- ????',
                       '???? box ????',
                       '???? --- ????'))

make_generic(44, 445, ('???? box ????',
                       '?????????????'))

make_generic(45, 545, ('?????????????',
                       '???? box ????',
                       '?????????????'))

# Textual (non programming) templates.

Template(111, 113, ('box',))

Template(112, 213, ('| box |',
                    '+-----+'))

Template(113, 313, ('+-----+',
                    '| box |',
                    '+-----+'))

Template(114, 413, ('| box |',
                    '*=====*'))

Template(115, 513, ('*=====*',
                    '| box |',
                    '*=====*'))

Template(121, 123, ('| box |',))

Template(122, 223, ('| box |',
                    '`-----\''))

Template(123, 323, ('.-----.',
                    '| box |',
                    '`-----\''))

Template(124, 423, ('| box |',
                    '\\=====/'))

Template(125, 523, ('/=====\\',
                    '| box |',
                    '\\=====/'))

Template(141, 143, ('| box ',))

Template(142, 243, ('* box *',
                    '*******'))

Template(143, 343, ('*******',
                    '* box *',
                    '*******'))

Template(144, 443, ('X box X',
                    'XXXXXXX'))

Template(145, 543, ('XXXXXXX',
                    'X box X',
                    'XXXXXXX'))
# C language templates.

Template(211, 118, ('/* box */',))

Template(212, 218, ('/* box */',
                    '/* --- */'))

Template(213, 318, ('/* --- */',
                    '/* box */',
                    '/* --- */'))

Template(214, 418, ('/* box */',
                    '/* === */'))

Template(215, 518, ('/* === */',
                    '/* box */',
                    '/* === */'))

Template(221, 128, ('/* ',
                    '   box',
                    '*/'))

Template(222, 228, ('/*    .',
                    '| box |',
                    '`----*/'))

Template(223, 328, ('/*----.',
                    '| box |',
                    '`----*/'))

Template(224, 428, ('/*    \\',
                    '| box |',
                    '\\====*/'))

Template(225, 528, ('/*====\\',
                    '| box |',
                    '\\====*/'))

Template(231, 138, ('/*    ',
                    ' | box',
                    ' */   '))

Template(232, 238, ('/*        ',
                    ' | box | ',
                    ' *-----*/'))

Template(233, 338, ('/*-----* ',
                    ' | box | ',
                    ' *-----*/'))

Template(234, 438, ('/* box */',
                    '/*-----*/'))

Template(235, 538, ('/*-----*/',
                    '/* box */',
                    '/*-----*/'))

Template(241, 148, ('/*    ',
                    ' * box',
                    ' */   '))

Template(242, 248, ('/*     * ',
                    ' * box * ',
                    ' *******/'))

Template(243, 348, ('/******* ',
                    ' * box * ',
                    ' *******/'))

Template(244, 448, ('/* box */',
                    '/*******/'))

Template(245, 548, ('/*******/',
                    '/* box */',
                    '/*******/'))

Template(251, 158, ('/* ',
                    ' * box',
                    ' */   '))

if __name__ == '__main__':
    main(*sys.argv[1:])
