;;; icicles-opt.el --- User options (variables) for Icicles
;;
;; Filename: icicles-opt.el
;; Description: User options (variables) for Icicles
;; Author: Drew Adams
;; Maintainer: Drew Adams
;; Copyright (C) 1996-2009, Drew Adams, all rights reserved.
;; Created: Mon Feb 27 09:22:14 2006
;; Version: 22.0
;; Last-Updated: Fri Jun 18 12:58:12 2010 (-0700)
;;           By: dradams
;;     Update #: 3740
;; URL: http://www.emacswiki.org/cgi-bin/wiki/icicles-opt.el
;; Keywords: internal, extensions, help, abbrev, local, minibuffer,
;;           keys, apropos, completion, matching, regexp, command
;; Compatibility: GNU Emacs: 20.x, 21.x, 22.x, 23.x
;;
;; Features that might be required by this library:
;;
;;   `cl', `el-swank-fuzzy', `ffap', `ffap-', `fuzzy-match',
;;   `hexrgb', `icicles-face', `kmacro', `levenshtein', `thingatpt',
;;   `thingatpt+', `wid-edit', `widget'.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;;  This is a helper library for library `icicles.el'.  It defines
;;  user options (variables).  For Icicles documentation, see
;;  `icicles-doc1.el' and `icicles-doc2.el'.
;;
;;  Widgets defined here:
;;
;;    `icicle-key-definition'.
;;
;;  Constants defined here:
;;
;;    `icicle-anychar-regexp'.
;;
;;  User options defined here (in Custom group `Icicles'):
;;
;;    `icicle-act-before-cycle-flag',
;;    `icicle-add-proxy-candidates-flag',
;;    `icicle-alternative-actions-alist',
;;    `icicle-alternative-sort-comparer',
;;    `icicle-apropos-complete-keys',
;;    `icicle-apropos-complete-no-display-keys',
;;    `icicle-apropos-cycle-next-keys',
;;    `icicle-apropos-cycle-next-action-keys',
;;    `icicle-apropos-cycle-next-alt-action-keys',
;;    `icicle-apropos-cycle-next-help-keys',
;;    `icicle-apropos-cycle-previous-keys',
;;    `icicle-apropos-cycle-previous-action-keys',
;;    `icicle-apropos-cycle-previous-alt-action-keys',
;;    `icicle-apropos-cycle-previous-help-keys',
;;    `icicle-anything-transform-candidates-flag',
;;    `icicle-bookmark-name-length-max',
;;    `icicle-bookmark-refresh-cache-flag', `icicle-buffer-configs',
;;    `icicle-buffer-extras',
;;    `icicle-buffer-ignore-space-prefix-flag',
;;    `icicle-buffer-match-regexp', `icicle-buffer-no-match-regexp',
;;    `icicle-buffer-predicate', `icicle-buffer-require-match-flag'
;;    `icicle-buffer-sort', `icicle-buffers-ido-like-flag',
;;    `icicle-candidate-width-factor',
;;    `icicle-change-region-background-flag',
;;    `icicle-change-sort-order-completion-flag',
;;    `icicle-C-l-uses-completion-flag', `icicle-color-themes',
;;    `icicle-comint-dynamic-complete-replacements',
;;    `icicle-command-abbrev-alist',
;;    `icicle-command-abbrev-match-all-parts-flag',
;;    `icicle-command-abbrev-priority-flag',
;;    `icicle-complete-key-anyway-flag',
;;    `icicle-complete-keys-self-insert-flag',
;;    `icicle-completing-read+insert-keys',
;;    `icicle-completion-history-max-length',
;;    `icicle-Completions-display-min-input-chars',
;;    `icicle-completions-format',
;;    `icicle-Completions-frame-at-right-flag',
;;    `icicle-Completions-text-scale-decrease',
;;    `icicle-Completions-window-max-height',
;;    `icicle-customize-save-flag',
;;    `icicle-customize-save-variable-function',
;;    `icicle-cycle-into-subdirs-flag',
;;    `icicle-cycling-respects-completion-mode',
;;    `icicle-default-thing-insertion', `icicle-default-value',
;;    `icicle-define-alias-commands-flag',
;;    `icicle-deletion-action-flag', `icicle-dot-show-regexp-flag',
;;    `icicle-dot-string', `icicle-expand-input-to-common-match-flag',
;;    `icicle-file-extras', `icicle-file-match-regexp',
;;    `icicle-file-no-match-regexp', `icicle-file-predicate',
;;    `icicle-file-require-match-flag', `icicle-file-sort',
;;    `icicle-files-ido-like-flag',
;;    `icicle-filesets-as-saved-completion-sets-flag',
;;    `icicle-guess-commands-in-path',
;;    `icicle-help-in-mode-line-flag',
;;    `icicle-hide-common-match-in-Completions-flag',
;;    `icicle-highlight-historical-candidates-flag',
;;    `icicle-highlight-input-completion-failure',
;;    `icicle-highlight-input-completion-failure-delay',
;;    `icicle-highlight-input-completion-failure-threshold',
;;    `icicle-highlight-input-initial-whitespace-flag',
;;    `icicle-highlight-lighter-flag', `icicle-ignored-directories',
;;    `icicle-ignore-space-prefix-flag',
;;    `icicle-incremental-completion-delay',
;;    `icicle-incremental-completion-flag',
;;    `icicle-incremental-completion-threshold',
;;    `icicle-inhibit-advice-functions', `icicle-inhibit-ding-flag',
;;    `icicle-input-string', `icicle-inter-candidates-min-spaces',
;;    `icicle-isearch-complete-keys', `icicle-key-complete-keys',
;;    `icicle-key-descriptions-use-<>-flag',
;;    `icicle-key-descriptions-use-angle-brackets-flag',
;;    `icicle-keymaps-for-key-completion', `icicle-kmacro-ring-max',
;;    `icicle-levenshtein-distance', `icicle-list-end-string',
;;    `icicle-list-join-string', `icicle-list-nth-parts-join-string',
;;    `icicle-mark-position-in-candidate', `icicle-max-candidates',
;;    `icicle-menu-items-to-history-flag',
;;    `icicle-minibuffer-setup-hook', `icicle-modal-cycle-down-keys',
;;    `icicle-modal-cycle-down-action-keys',
;;    `icicle-modal-cycle-down-alt-action-keys',
;;    `icicle-modal-cycle-down-help-keys',
;;    `icicle-modal-cycle-up-keys',
;;    `icicle-modal-cycle-up-action-keys',
;;    `icicle-modal-cycle-up-alt-action-keys',
;;    `icicle-modal-cycle-up-help-keys', `icicle-no-match-hook',
;;    `icicle-option-type-prefix-arg-list',
;;    `icicle-point-position-in-candidate',
;;    `icicle-populate-interactive-history-flag',
;;    `icicle-pp-eval-expression-print-length',
;;    `icicle-pp-eval-expression-print-level',
;;    `icicle-prefix-complete-keys',
;;    `icicle-prefix-complete-no-display-keys',
;;    `icicle-prefix-cycle-next-keys',
;;    `icicle-prefix-cycle-next-action-keys',
;;    `icicle-prefix-cycle-next-alt-action-keys',
;;    `icicle-prefix-cycle-next-help-keys',
;;    `icicle-prefix-cycle-previous-keys',
;;    `icicle-prefix-cycle-previous-action-keys',
;;    `icicle-prefix-cycle-previous-alt-action-keys',
;;    `icicle-prefix-cycle-previous-help-keys',
;;    `icicle-previous-candidate-keys',
;;    `icicle-quote-shell-file-name-flag',
;;    `icicle-read+insert-file-name-keys',
;;    `icicle-redefine-standard-commands-flag',
;;    `icicle-regexp-quote-flag', `icicle-regexp-search-ring-max',
;;    `icicle-region-background', `icicle-require-match-flag',
;;    `icicle-saved-completion-sets', `icicle-search-cleanup-flag',
;;    `icicle-search-context-match-predicate',
;;    `icicle-search-from-isearch-keys',
;;    `icicle-search-highlight-all-current-flag',
;;    `icicle-search-highlight-context-levels-flag',
;;    `icicle-search-highlight-threshold', `icicle-search-hook',
;;    `icicle-search-replace-common-match-flag',
;;    `icicle-search-replace-literally-flag',
;;    `icicle-search-replace-whole-candidate-flag',
;;    `icicle-search-ring-max', `icicle-search-whole-word-flag',
;;    `icicle-shell-command-candidates-cache',
;;    `icicle-show-Completions-help-flag',
;;    `icicle-show-Completions-initially-flag',
;;    `icicle-show-multi-completion-flag', `icicle-sort-comparer',
;;    `icicle-sort-orders-alist', `icicle-special-candidate-regexp',
;;    `icicle-S-TAB-completion-methods-alist',
;;    `icicle-swank-prefix-length', `icicle-swank-timeout',
;;    `icicle-TAB-completion-methods',
;;    `icicle-TAB-shows-candidates-flag',
;;    `icicle-test-for-remote-files-flag',
;;    `icicle-thing-at-point-functions',
;;    `icicle-top-level-key-bindings',
;;    `icicle-top-level-when-sole-completion-delay',
;;    `icicle-top-level-when-sole-completion-flag',
;;    `icicle-touche-pas-aux-menus-flag', `icicle-transform-function',
;;    `icicle-type-actions-alist',
;;    `icicle-unpropertize-completion-result-flag',
;;    `icicle-update-input-hook', `icicle-use-~-for-home-dir-flag',
;;    `icicle-use-C-for-actions-flag',
;;    `icicle-use-anything-candidates-flag',
;;    `icicle-use-candidates-only-once-flag',
;;    `icicle-word-completion-keys',
;;    `icicle-WYSIWYG-Completions-flag', `icicle-yank-function'.
;;
;;  Functions defined here:
;;
;;    `icicle-bind-top-level-commands',
;;    `icicle-buffer-sort-*...*-last',
;;    `icicle-compute-shell-command-candidates',
;;    `icicle-increment-color-value', `icicle-remap'.
;;
;;  For descriptions of changes to this file, see `icicles-chg.el'.
;;
;;  Note: Occasionally I have renamed or removed an Icicles option.
;;  If you have customized such an option, then your customization
;;  will no longer have any effect.  With the exception of options
;;  `icicle-mode' and `icicle-mode-hook', library `icicles-opt.el'
;;  always contains the complete set of Icicles options.  If your
;;  custom file or init file contains an Icicles option that is not
;;  listed above, then you can remove it because it is obsolete.
 
;;(@> "Index")
;;
;;  If you have library `linkd.el' and Emacs 22 or later, load
;;  `linkd.el' and turn on `linkd-mode' now.  It lets you easily
;;  navigate around the sections of this doc.  Linkd mode will
;;  highlight this Index, as well as the cross-references and section
;;  headings throughout this file.  You can get `linkd.el' here:
;;  http://dto.freeshell.org/notebook/Linkd.html.
;;
;;  (@> "User options, organized alphabetically, except for dependencies")
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or (at
;; your option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Code:

(require 'hexrgb nil t)     ;; (no error if not found): hexrgb-color-values-to-hex,
                            ;; hexrgb-increment-(red|green|blue), hexrgb-rgb-to-hsv,
                            ;; hexrgb-color-values-to-hex, hexrgb-hsv-to-rgb
(require 'thingatpt)        ;; symbol-at-point, thing-at-point, thing-at-point-url-at-point,
(require 'thingatpt+ nil t) ;; (no error if not found): symbol-name-nearest-point,
                            ;; word-nearest-point
(require 'icicles-face)     ;; icicle-increment-color-hue.

;; Quiet the byte-compiler.
(defvar shell-completion-execonly)      ; In `shell.el'.

(defvar icicle-mode-map)
(defvar icicle-dot-string-internal)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 
;;(@* "User options, organized alphabetically, except for dependencies")

;;; User options, organized alphabetically, except for dependencies --

;;;###autoload
(defcustom icicle-act-before-cycle-flag nil
  "*Non-nil means act on current candidate, then cycle to next/previous.
Otherwise (nil), cycle to the next or previous candidate, and then act
on it.

This affects keys such as the following\\<minibuffer-local-completion-map>:
`C-down', `C-up', `C-next', `C-prior', \
`\\[icicle-help-on-next-prefix-candidate]', `\\[icicle-help-on-previous-prefix-candidate]',
`\\[icicle-help-on-next-apropos-candidate]', `\\[icicle-help-on-previous-apropos-candidate]', \
`\\[icicle-next-prefix-candidate-alt-action]', \
`\\[icicle-previous-prefix-candidate-alt-action]', \
`\\[icicle-next-apropos-candidate-alt-action]', and
`\\[icicle-previous-apropos-candidate-alt-action]'.

Note: A few Icicles commands ignore this setting, in order to \"do the
right thing\"."
  :type 'boolean :group 'Icicles-Key-Bindings :group 'Icicles-Miscellaneous)

;;;###autoload
(defcustom icicle-add-proxy-candidates-flag nil ; Toggle with `C-M-_'.
  "*Non-nil means to include proxy candidates whenever possible.
A proxy candidate is a special candidate (shown in *Completions* using
face `icicle-special-candidate') whose name is a placeholder for the
real candidate.  The proxy candidate typically stands for some value
obtained from the cursor position or by some action such as clicking
the mouse.  Example candidates include a color or file name, named by
proxy candidates such as `*copied foreground*' or `*file at point*'.

You can toggle this option at any time from the minibuffer using
`\\<minibuffer-local-completion-map>\\[icicle-toggle-proxy-candidates]'.  However, for \
commands that provide many proxy candidates, if
the flag is off initially when input is read, then you must re-invoke
the completing command for the new value to take effect.  (This is for
performance reasons.)"
  :type 'boolean :group 'Icicles-Matching)

;;;###autoload
(defcustom icicle-alternative-actions-alist ()
  "*Alist of Emacs commands and alternative action functions.
This always overrides any alternative action defined by
`icicle-candidate-alt-action-fn'.

Each alist element has the form (COMMAND . FUNCTION), where COMMAND is
a command (a symbol) that reads input and FUNCTION is the
alternative-action function it uses.  To disable alternative action
for a given command, use `ignore' as the FUNCTION.

This option has no effect on `icicle-all-candidates-list-alt-action',
that is, `M-|', but it does affect `C-|'."
  :type '(alist
          :key-type   (symbol   :tag "Command")
          :value-type (function :tag "Alternative action (function)"))
  :group 'Icicles-Miscellaneous)

;;;###autoload
(defcustom icicle-alternative-sort-comparer ; Toggle with `C-M-,'.
  'icicle-historical-alphabetic-p
  "*An alternative sort function, in place of `icicle-sort-comparer'.
You can swap this with `icicle-sort-comparer' at any time by using
`icicle-toggle-alternative-sorting' (\\<minibuffer-local-completion-map>\
`\\[icicle-toggle-alternative-sorting]' in the minibuffer)."
  :type '(choice (const :tag "None" nil) function) :group 'Icicles-Completions-Display)

;; Must be before `icicle-dot-string'.
(defconst icicle-anychar-regexp (let ((strg  (copy-sequence "\\(.\\|[\n]\\)")))
                                  (set-text-properties 0 (length strg)
                                                       '(display "." face highlight)
                                                       strg)
                                  strg)
  "Regexp that matches any single character, including newline.")

;;;###autoload
(defcustom icicle-anything-transform-candidates-flag nil
  "*Non-nil means `icicle-anything' transforms completion candidates.
Function `anything-transform-candidates' is used for the transforming.

The advantage of a nil value is that `icicle-anything' then acts as a
multi-command: you can act on multiple candidates, or apply multiple
actions for the same candidate, within a single invocation of
`icicle-anything' (or related commands).

The advantage of a non-nil value is that some of the displayed
Anything candidates might be more readable.

This option has no effect if library `anything.el' cannot be loaded."
  :type 'boolean :group 'Icicles-Completions-Display :group 'Icicles-Matching)

;;;###autoload
(defcustom icicle-apropos-complete-keys '([S-tab] [S-iso-lefttab])
  ;; In Emacs 22 and later, `backtab' is the canonical key that represents both `S-tab' and
  ;; `S-iso-lefttab', so in principle that could be used in the default value for Emacs 22+.
  ;;
  ;; In other words, the following should be sufficient:
  ;;   (if (> emacs-major-version 21)
  ;;       '([backtab])
  ;;     '([S-tab] [S-iso-lefttab]))
  ;;
  ;; However, some Emacs 22+ libraries, such as `info.el', are brain-dead and explicitly
  ;; bind both `backtab' and `S-tab'.  I filed Emacs bug #1281.
  "*Key sequences to use for `icicle-apropos-complete'.
A list of values that each has the same form as a key-sequence
argument to `define-key'.  It is a list mainly in order to accommodate
different keyboards - for example, `S-tab' and `S-iso-lefttab'."
  :type '(repeat sexp) :group 'Icicles-Key-Bindings)

;;;###autoload
(defcustom icicle-apropos-complete-no-display-keys '([C-M-S-tab] [C-M-S-iso-lefttab])
  "*Key sequences to use for `icicle-apropos-complete-no-display'.
A list of values that each has the same form as a key-sequence
argument to `define-key'.  It is a list mainly in order to accommodate
different keyboards - for example, `C-M-S-tab' and `C-M-S-iso-lefttab'."
  :type '(repeat sexp) :group 'Icicles-Key-Bindings)

;;;###autoload
(defcustom icicle-apropos-cycle-next-keys '([next])
  "*Key sequences for apropos completion to cycle to the next candidate.
A list of values that each has the same form as a key-sequence
argument to `define-key'.  It is a list mainly in order to accommodate
different keyboards.

Option `icicle-use-C-for-actions-flag' swaps these keys with
`icicle-apropos-cycle-next-action-keys'."
  :type '(repeat sexp) :group 'Icicles-Key-Bindings)

;;;###autoload
(defcustom icicle-apropos-cycle-next-action-keys '([C-next])
  "*Keys for apropos completion to cycle next and perform action.
A list of values that each has the same form as a key-sequence
argument to `define-key'.  It is a list mainly in order to accommodate
different keyboards.

Option `icicle-use-C-for-actions-flag' swaps these keys with
`icicle-apropos-cycle-next-keys'."
  :type '(repeat sexp) :group 'Icicles-Key-Bindings)

;;;###autoload
(defcustom icicle-apropos-cycle-next-alt-action-keys '([C-S-next])
  "*Keys for apropos completion to cycle next and perform alt action.
A list of values that each has the same form as a key-sequence
argument to `define-key'.  It is a list mainly in order to accommodate
different keyboards."
  :type '(repeat sexp) :group 'Icicles-Key-Bindings)

;;;###autoload
(defcustom icicle-apropos-cycle-next-help-keys '([(control meta next)]) ; `C-M-next'
  "*Keys for apropos completion to cycle next and show candidate help.
A list of values that each has the same form as a key-sequence
argument to `define-key'.  It is a list mainly in order to accommodate
different keyboards."
  :type '(repeat sexp) :group 'Icicles-Key-Bindings)

;;;###autoload
(defcustom icicle-apropos-cycle-previous-keys '([prior])
  "*Key sequences for apropos completion to cycle to the previous candidate.
A list of values that each has the same form as a key-sequence
argument to `define-key'.  It is a list mainly in order to accommodate
different keyboards.

Option `icicle-use-C-for-actions-flag' swaps these keys with
`icicle-apropos-cycle-previous-action-keys'."
  :type '(repeat sexp) :group 'Icicles-Key-Bindings)

;;;###autoload
(defcustom icicle-apropos-cycle-previous-action-keys '([C-prior])
  "*Keys for apropos completion to cycle previous and perform action.
A list of values that each has the same form as a key-sequence
argument to `define-key'.  It is a list mainly in order to accommodate
different keyboards.

Option `icicle-use-C-for-actions-flag' swaps these keys with
`icicle-apropos-cycle-previous-keys'."
  :type '(repeat sexp) :group 'Icicles-Key-Bindings)

;;;###autoload
(defcustom icicle-apropos-cycle-previous-alt-action-keys '([C-S-prior])
  "*Keys for apropos completion to cycle previous and perform alt action.
A list of values that each has the same form as a key-sequence
argument to `define-key'.  It is a list mainly in order to accommodate
different keyboards."
  :type '(repeat sexp) :group 'Icicles-Key-Bindings)

;;;###autoload
(defcustom icicle-apropos-cycle-previous-help-keys '([(control meta prior)]) ; `C-M-prior'
  "*Keys for apropos completion to cycle previous and show candidate help.
A list of values that each has the same form as a key-sequence
argument to `define-key'.  It is a list mainly in order to accommodate
different keyboards."
  :type '(repeat sexp) :group 'Icicles-Key-Bindings)

;;;###autoload
(defcustom icicle-bookmark-name-length-max 70
  "*Maximum number of characters used to name a bookmark.
When `icicle-bookmark-cmd' is used with a non-negative numeric prefix
arg, the name of the bookmark that is set has at most this many chars."
  :type 'integer :group 'Icicles-Miscellaneous)

;;;###autoload
(defcustom icicle-bookmark-refresh-cache-flag t
  "*t means `icicle-bookmark' refreshes the bookmark-list cache.
Use nil to speed up `icicle-bookmark(-other-window)' if you have a lot
of bookmarks, at the cost of having the bookmark list possibly not be
up to date.  Use t if you want to be sure the list is refreshed.

If nil, the list of bookmarks is updated only if you use `C-u'.
If t, the list is always updated unless you use `C-u'.

This affects only commands such as `icicle-bookmark' that use the full
bookmark list.  It does not affect more specific Icicles bookmark
commands such as `\\[icicle-bookmark-dired-other-window]' or the use
of a negative prefix arg with
`\\[icicle-bookmark-cmd]'.

Regardless of the option value, the cache is refreshed whenever you
use `S-delete' to delete a candidate bookmark."
  :type 'boolean :group 'Icicles-Completions-Display :group 'Icicles-Matching)

;;;###autoload
(defcustom icicle-buffer-extras nil
  "*List of additional buffer-name candidates added to the normal list.
List elements are strings."
  :type '(repeat string) :group 'Icicles-Buffers :group 'Icicles-Matching)

;;;###autoload
(defcustom icicle-buffer-ignore-space-prefix-flag t
  "*Override `icicle-ignore-space-prefix-flag' for `icicle-buffer*'.
Note: This option is provided mainly for use (binding) in
`icicle-define-command' and `icicle-define-file-command'.
You probably do not want to set this globally, but you can."
  :type 'boolean :group 'Icicles-Buffers :group 'Icicles-Matching)

;;;###autoload
(defcustom icicle-buffer-match-regexp nil
  "*nil or a regexp that buffer-name completion candidates must match.
If nil, then this does nothing.  If a regexp, then show only
candidates that match it (and match the user input).
See also `icicle-buffer-no-match-regexp'."
  :type '(choice (const :tag "None" nil) regexp)
  :group 'Icicles-Buffers :group 'Icicles-Matching)

;;;###autoload
(defcustom icicle-buffer-no-match-regexp nil
  "*nil or a regexp that buffer-name completion candidates must not match.
If nil, then this does nothing.  If a regexp, then show only
candidates that do not match it.
See also `icicle-buffer-match-regexp'."
  :type '(choice (const :tag "None" nil) regexp)
  :group 'Icicles-Buffers :group 'Icicles-Matching)

;;;###autoload
(defcustom icicle-buffer-predicate nil
  "*nil or a predicate that buffer-name candidates must satisfy.
If nil, then this does nothing.  Otherwise, this is a function of one
argument, a candidate, and only candidates that satisfy the predicate
are displayed.  For example, this value will show only buffers that
are associated with files:

  (lambda (bufname) (buffer-file-name (get-buffer bufname)))"
  :type '(choice (const :tag "None" nil) function)
  :group 'Icicles-Buffers :group 'Icicles-Matching)

;;;###autoload
(defcustom icicle-buffer-require-match-flag nil
  "*Override `icicle-require-match-flag' for `icicle-buffer*' commands.
Controls the REQUIRE-MATCH arg to `completing-read' and `read-file-name'.
The possible values are as follows:
- nil means this option imposes nothing on completion;
  the REQUIRE-MATCH argument provided to the function governs behavior
- `no-match-required' means the same as a nil value for REQUIRE-MATCH
- `partial-match-ok' means the same as a t value for REQUIRE-MATCH
- `full-match-required' means the same as a non-nil, non-t value for
  REQUIRE-MATCH

Note: This option is provided mainly for use (binding) in
`icicle-define-command' and `icicle-define-file-command'.
You probably do not want to set this globally, but you can."
  :type '(choice
          (const :tag "Do not impose any match behavior"   nil)
          (const :tag "Do not require a match"             no-match-required)
          (const :tag "Require a partial match, with RET"  partial-match-ok)
          (const :tag "Require a full match"               full-match-required))
  :group 'Icicles-Buffers :group 'Icicles-Matching)

;;;###autoload
(defcustom icicle-buffer-sort 'icicle-buffer-sort-*...*-last
  "*A sort function for buffer names, or nil.
Examples of sort functions are `icicle-buffer-sort-*...*-last' and
`string<'.  If nil, then buffer names are not sorted.  Option
`icicle-sort-comparer' is bound to `icicle-buffer-sort' by command
`icicle-buffer'."
  :type '(choice (const :tag "None" nil) function)
  :group 'Icicles-Buffers :group 'Icicles-Completions-Display)

;;;###autoload
(defcustom icicle-buffers-ido-like-flag nil
  "t means `icicle-buffer' and similar commands act more Ido-like.
Specifically, those commands then bind these options to t:
 `icicle-show-Completions-initially-flag'
 `icicle-top-level-when-sole-completion-flag'
 `icicle-default-value'"
  :type 'boolean
  :group 'Icicles-Buffers :group 'Icicles-Completions-Display :group 'Icicles-Matching)

;;;###autoload
(defcustom icicle-candidate-width-factor 80
  "*Percentage of widest candidate width to use for calculating columns.
The number of columns of candidates displayed in *Completions* is no
more than the window width divided by this percentage of the maximum
candidate width.

Increasing this toward 100 spreads columns out. Decreasing it
compresses columns together.  The higher the value, the more
candidates will form well-defined columns, but the likelier that
horizontal space will be wasted between them.  The lower the value,
the more candidates will not line up in columns, but the less
horizontal space will be wasted between them.

When most candidates are almost as wide as the widest candidate, a
high value works well.  When most candidates are much shorter than the
widest candidate, a low value works well.

If you use Do Re Mi (library `doremi.el'), then you can modify this
option incrementally during completion, seeing the effect as it
changes.  Use `C-x w' from the minibuffer, then use the `right' and
`left' arrow keys or the mouse wheel to increment and decrement the
value.  WYSIWYG.

See also option `icicle-inter-candidates-min-spaces' and (starting
with Emacs 23) option `icicle-Completions-text-scale-decrease'."
  :type 'integer :group 'Icicles-Completions-Display)

;; Must be before `icicle-change-region-background-flag'.
;;;###autoload
(defcustom icicle-mark-position-in-candidate 'input-end
  "*Position of mark when you cycle through completion candidates.
This is the mark position in the minibuffer.
Possible values are those for `icicle-point-position-in-candidate'."
  :type '(choice
          (const :tag "Leave mark at the beginning of the minibuffer input"  input-start)
          (const :tag "Leave mark at the end of the minibuffer input"        input-end)
          (const :tag "Leave mark at the beginning of the completion root"   root-start)
          (const :tag "Leave mark at the end of the completion root"         root-end))
  :group 'Icicles-Minibuffer-Display)

;; Must be before `icicle-change-region-background-flag'.
;;;###autoload
(defcustom icicle-point-position-in-candidate 'root-end
  "*Position of cursor when you cycle through completion candidates.
This is the cursor position in the minibuffer.
Possible values are:
 `input-start': beginning of the minibuffer input
 `input-end':   end of the minibuffer input
 `root-start':  beginning of the completion root
 `root-end':    end of the completion root
When input is expected to be a file name, `input-start' is just after
the directory, which is added automatically during completion cycling.
See also `icicle-mark-position-in-candidate'."
  :type '(choice
          (const :tag "Leave cursor at the beginning of the minibuffer input"  input-start)
          (const :tag "Leave cursor at the end of the minibuffer input"        input-end)
          (const :tag "Leave cursor at the beginning of the completion root"   root-start)
          (const :tag "Leave cursor at the end of the completion root"         root-end))
  :group 'Icicles-Minibuffer-Display)

;;;###autoload
(defcustom icicle-change-region-background-flag
  (not (eq icicle-point-position-in-candidate icicle-mark-position-in-candidate))
  "*Non-nil means use color `icicle-region-background' during input.
See `icicle-region-background'.  If you load library `hexrgb.el'
before Icicles, then `icicle-region-background' will be a slightly
different hue from your normal background color.  This makes
minibuffer input easier to read than if your normal `region' face were
used.  This has an effect only during minibuffer input.  A non-nil
value for this option is particularly useful if you use
delete-selection mode."
  :type 'boolean :group 'Icicles-Minibuffer-Display)

;;;###autoload
(defcustom icicle-change-sort-order-completion-flag nil
  "*Non-nil means `icicle-change-sort-order' uses completion, by default.
Otherwise, it cycles among the possible sort orders.  You can override
the behavior by using `C-u' with `icicle-change-sort-order'."
  :type 'boolean :group 'Icicles-Completions-Display :group 'Icicles-Matching)

;;;###autoload
(defcustom icicle-C-l-uses-completion-flag nil
  "*Non-nil means \\<minibuffer-local-completion-map>\
`\\[icicle-retrieve-previous-input]' uses completion for choosing completion history
entries, by default.  Otherwise, it cycles among the possible previous
inputs.  You can override the behavior by using `C-u' with `\\[icicle-retrieve-previous-input]'."
  :type 'boolean :group 'Icicles-Minibuffer-Display :group 'Icicles-Matching)

;; Replace this list by your favorite color themes. Each must be the name of a defined function.
;; By default, this includes all color themes defined globally (variable `color-themes').
;;
;; NOTE: We need the `condition-case' because of a BUG in `directory-files' for Emacs 20.
;; Bug reported to `color-theme.el' maintainer 2009-11-22.  The problem is that the default value
;; of `color-theme-libraries' concats `file-name-directory', which ends in `/', with `/themes',
;; not with `themes'.  So the result is `...//themes'.  That is tolerated by Emacs 21+
;; `directory-files', but not for Emacs 20.  Until this `color-theme.el' bug is fixed, Emacs 20
;; users will need to manually load `color-theme-libraries.el'.
;;;###autoload
(defcustom icicle-color-themes ()
  "*List of color themes to cycle through using `M-x icicle-color-theme'.
Note: Starting with Color Theme version 6.6.0, you will need to put
library `color-theme-library.el', as well as library `color-theme.el',
in your `load-path'."
  :type 'hook :group 'Icicles-Miscellaneous)

;;;###autoload
(defcustom icicle-comint-dynamic-complete-replacements
  '((comint-dynamic-complete-filename    'icicle-comint-dynamic-complete-filename)
    (shell-dynamic-complete-command      'icicle-shell-dynamic-complete-command)
    (shell-dynamic-complete-environment-variable
     'icicle-shell-dynamic-complete-environment-variable)
    (shell-dynamic-complete-filename     'icicle-shell-dynamic-complete-filename)
    (ess-complete-filename               'icicle-ess-complete-filename)
    (ess-complete-object-name            'icicle-ess-complete-object-name)
    )
  "*List of function replacements for `comint-dynamic-complete-functions'.
Instead of using `comint-dynamic-complete-functions' as is, command
`icicle-comint-dynamic-complete' replaces functions in that list
according to the value of this option.

Each option list element is itself a list of two elements.  The first
is a function to replace (a symbol), and the second is the replacement
function (a sexp that evaluates to a function).  For example, this
list element says to replace completion function `foo' by completion
function `my-foo': (foo 'my-foo).

You can use this option to provide Icicles completion for various
modes that inherit from Comint mode or otherwise use
`comint-dynamic-complete'."
  :type '(repeat (list symbol sexp)) :group 'Icicles-Miscellaneous)

;;;###autoload
(defcustom icicle-command-abbrev-alist ()
  "*Alist of command abbreviations and commands, with frequency of use.
Each element has the form (COMMAND ABBREV N), where ABBREV is an
abbreviation of COMMAND and N is the number of times COMMAND has been
invoked via ABBREV.  Both COMMAND and ABBREV are symbols."
  :type '(alist :key-type symbol :value-type (list symbol integer)) :group 'Icicles-Matching)

;;;###autoload
(defcustom icicle-command-abbrev-match-all-parts-flag nil
  "*Non-nil means `icicle-command-abbrev' matches each command-name part.
Otherwise, an abbrev need match only a prefix of the command name."
  :type 'boolean :group 'Icicles-Matching)

;;;###autoload
(defcustom icicle-command-abbrev-priority-flag nil
  "*nil means commands take precedence over abbreviations for `\\<icicle-mode-map>\
\\[icicle-command-abbrev]'."
  :type 'boolean :group 'Icicles-Matching)

;;;###autoload
(defcustom icicle-complete-key-anyway-flag nil
  "*Non-nil means bind `S-TAB' for key completion even if already
bound.  If nil, then each of the keys in `icicle-key-complete-keys' is
bound to `icicle-complete-keys' in each keymap of
`icicle-keymaps-for-key-completion' only if `S-TAB' is not already
bound in the keymap.

Note: the keys in `icicle-key-complete-keys' are always bound to
`icicle-complete-keys' in `icicle-mode-map'.  This option affects only
the binding of those keys in `icicle-keymaps-for-key-completion'."
  :type 'boolean :group 'Icicles-Key-Completion :group 'Icicles-Key-Bindings)

;;;###autoload
(defcustom icicle-complete-keys-self-insert-flag nil
  "*Non-nil means `icicle-complete-keys' includes self-inserting keys.
That means keys bound to `self-insert-command'."
  :type 'boolean :group 'Icicles-Key-Completion)

;;;###autoload
(defcustom icicle-completing-read+insert-keys '([(control meta shift ?c)])
  "*Key sequences to invoke `icicle-completing-read+insert'.
A list of values that each has the same form as a key-sequence
argument to `define-key'.  It is a list mainly in order to accommodate
different keyboards.

Such a key has no effect unless
`icicle-completing-read+insert-candidates' is non-nil."
  :type '(repeat sexp) :group 'Icicles-Key-Bindings)

;;;###autoload
(defcustom icicle-completion-history-max-length (if icicle-C-l-uses-completion-flag 1000 100)
  "*Maximum number of inputs to save in the completion history.
This is the history that you access using \\<minibuffer-local-completion-map>\
`\\[icicle-retrieve-previous-input]' and `\\[icicle-retrieve-next-input]'."
  :type 'integer :group 'Icicles-Miscellaneous)

;;;###autoload
(defcustom icicle-Completions-display-min-input-chars 0
  "**Completions* window is removed if fewer chars than this are input.
You might want to set this to, say 1 or 2, to avoid display of a large
set of candidates during incremental completion.  The default value of
0 causes this option to have no effect: *Completions* is never removed
based only on the number of input characters."
  :type 'integer :group 'Icicles-Completions-Display)

;;;###autoload
(defcustom icicle-completions-format (if (boundp 'completions-format)
                                         completions-format ; Defined in Emacs 23+.
                                       'horizontal)  
  "*Layout of completion candidates in buffer *Completions*.
`vertical' means display down columns first, then to the right.
`horizontal' or nil means display across rows first, then down.

A `vertical' value is overridden (ignored) when multi-line
multi-completions are used.  For clarity, the layout for such
multi-completions is always horizontal."
  :type '(choice
          (const :tag "Display vertically"    vertical)
          (other :tag "Display horizontally"  horizontal))
  :group 'Icicles-Completions-Display)

;;;###autoload
(defcustom icicle-Completions-frame-at-right-flag t
  "*Non-nil means move *Completions* frame to right edge of display.
This is done by `icicle-candidate-action'.
It only happens if *Completions* is alone in its frame.
This can be useful to make *Completions* more visible."
  :type 'boolean :group 'Icicles-Completions-Display)

;;;###autoload
(when (fboundp 'text-scale-decrease)    ; Emacs 23+
  (defcustom icicle-Completions-text-scale-decrease 0.66
    "*Initial height decrease for text in buffer `*Completions*'.
A value of 0.0 means the height is not decreased at all.
This is used as the argument to function `text-scale-decrease'.
If you use library `doremi-frm.el', you can use `C-x -' to
incrementally resize the text during completion.

See also options `icicle-candidate-width-factor' and
`icicle-inter-candidates-min-spaces'."
    :type 'number :group 'Icicles-Completions-Display))

;;;###autoload
(defcustom icicle-Completions-window-max-height 30
  "*Maximum height of *Completions* window, in lines.
The window is fit to the buffer size, with this as maximum height.
Not used if *Completions* is a special buffer with its own frame.
Not used in Emacs releases prior to 21."
  :type 'integer :group 'Icicles-Completions-Display)

;;;###autoload
(defcustom icicle-customize-save-flag t
  "*Non-nil means save some updated Icicles options when you quit Emacs.
That is, add some functions to `kill-emacs-hook' that call
`customize-save-variable'.  Currently, this includes only function
`icicle-command-abbrev-save', which saves updated option
`icicle-command-abbrev-alist'."
  :type 'boolean :group 'Icicles-Minibuffer-Display)

;;;###autoload
(defcustom icicle-customize-save-variable-function 'customize-save-variable
  "*Function used to save user option changes.
I RECOMMEND that you do NOT change this.

The option value is a function that has the same signature as
`customize-save-variable' (perhaps with additional arguments after VAR
and VAL, the variable to save and its new value.

If you do not want changes that Icicles commands make to certain user
options to be saved automatically, you can set this to the function
\(symbol) `ignore'.  If you want to use your own function to somehow
save the current value, you can set this to your function."
  :type 'function :group 'Icicles-Miscellaneous)

;;;###autoload
(defcustom icicle-cycle-into-subdirs-flag nil
  "*Non-nil means minibuffer-input cycling explores subdirectories.
If this is non-nil, then you might want to use a function such as
`icicle-dirs-last-p' for option `icicle-sort-comparer', to prevent
cycling into subdirectories depth first.  Command
`icicle-sort-by-directories-last' does that."
  :type 'boolean :group 'Icicles-Miscellaneous)

;;;###autoload
(defcustom icicle-cycling-respects-completion-mode nil
  "*Non-nil means candidate cycling respects the current completion mode.
The default value of nil means that you use two separate sets of keys for
cycling apropos and prefix completions:
 - `icicle-apropos-cycle-previous-keys' (backward apropos)
   `icicle-apropos-cycle-next-keys'     (forward apropos)
 - `icicle-prefix-cycle-previous-keys'  (backward prefix)
   `icicle-prefix-cycle-next-keys'      (forward prefix)

Non-nil means that you can use a single set of keys (`up' and `down',
by default) to cycle both kinds of completion.  The keys are the
values of `icicle-modal-cycle-up-keys' (backward) and
`icicle-modal-cycle-down-keys' (forward).

The completion mode, and hence the behavior of these keys, is changed
whenever you hit `TAB' or `S-TAB' during completion: the mode is
prefix completion after `TAB' and `apropos' completion after `S-TAB'.

Before you hit `TAB' or `S-TAB', the cycling behavior depends on the
particular non-nil value of the option:

- `prefix'  means cycle prefix completions
- `apropos' means cycle apropos completions
- Any other non-nil value means cycle inputs from the input history

For example, if the value is `apropos' then you can immediately cycle
apropos completions without first hitting `S-TAB'.

Once you have used `TAB' or `S-TAB', the only way to traverse the
history is using `M-p' and `M-n' - `up' and `down' will cycle
completions.

Note: If the option is non-nil you can still use `M-p' and `M-n' to
traverse the input history, and `prior' and `next' to cycle apropos
completions (assuming that those default keys have not been changed).

And if you customize either the modal cycling keys or the prefix
cycling keys so that they are different (e.g. one of those sets is no
longer `up'/`down'), then you can also still use the latter.  In this
case, you need not use `TAB' and `S-TAB' to switch between the two
completion types, even when this option is non-nil - you can use the
separate apropos and prefix cycling keys.

After you change the value of this option, toggle Icicle mode off,
then on again, for the change to take effect in the same session."
  :type '(choice
          (const :tag "No"                                       nil)
          (const :tag "Yes, and use prefix cycling by default"   prefix)
          (const :tag "Yes, and use apropos cycling by default"  apropos)
          (other :tag "Yes, and use history cycling by default"  t))
  :group 'Icicles-Key-Bindings)

;;;###autoload
(defcustom icicle-default-thing-insertion 'alternatives
  "*Behavior of successive `\\<minibuffer-local-map>\\[icicle-insert-string-at-point]'.
If `alternatives', then the next function in the `car' of
`icicle-thing-at-point-functions' is used to retrieve the text to be
inserted.
If `more-of-the-same', then the function that is the `cdr' of
`icicle-thing-at-point-functions' is used to retrieve the text to be
inserted."
  :type `(choice
          (const :tag ,(substitute-command-keys
                        "Successive calls to `\\<minibuffer-local-map>\
\\[icicle-insert-string-at-point]' use different text-grabbing functions.")
           alternatives)
          (const :tag ,(substitute-command-keys
                        "Successive calls to `\\<minibuffer-local-map>\
\\[icicle-insert-string-at-point]' grab more text at point.")
           more-of-the-same))
  :group 'Icicles-Key-Bindings)

;; We don't use `define-obsolete-variable-alias' so that byte-compilation in older Emacs
;; works for newer Emacs too.
(when (fboundp 'defvaralias)            ; Emacs 22+
  (defvaralias 'icicle-init-value-flag 'icicle-default-value)
  (make-obsolete-variable 'icicle-init-value-flag 'icicle-default-value))

;;;###autoload
(defcustom icicle-default-value t
  "*How to treat the default value when reading minibuffer input.

When the default value argument to functions such as
`completing-read', `read-file-name', `read-from-minibuffer', and
`read-string' is non-nil and the initial-input argument is nil or
\"\", the default value can be added to the prompt as a hint or
inserted into the minibuffer as the initial input.

Adding it to the prompt is the default behavior and corresponds to the
behavior of vanilla Emacs.

Inserting the default value in the minibuffer as the initial input has
the advantage of not requiring you to use `M-n' to retrieve it.  It
has the disadvantage of making you use `M-p' (or do something else) to
get rid of the default value in the minibuffer if you do not want to
use or edit it.  If you often want to use or edit the default value,
then set `icicle-default-value' to non-nil and non-t.  If you rarely
do so, then set it to nil or t.

If inserted in the minibuffer, the value of this option also
determines whether or not the inserted text is preselected and where
the cursor is left: at the beginning or end of the text.

These are the possible option values:

  nil               - Do not insert default value or add it to prompt.
  t                 - Add default value to prompt.  Do not insert it.
  `insert-start'    - Insert default value and leave cursor at start.
  `insert-end'      - Insert default value and leave cursor at end.
  `preselect-start' - Insert and preselect default value;
                      leave cursor at beginning.
  `preselect-end'   - Insert and preselect default value;
                      leave cursor at end.

My own preference is `insert-end'.

Preselection can be useful in Delete Selection mode or PC Selection
mode.  It makes it easy to replace the value by typing characters, or
delete it by hitting `C-d' or `DEL' (backspace).  However, all of the
initial input is lost if you type or hit `C-d' or `DEL'.  That is
inconvenient if you want to keep most of it and edit it only slightly."
  :type '(choice
          (const :tag "Do not insert default value or add it to prompt"            nil)
          (const :tag "Add default value to prompt (do not insert in minibuffer)"  t)
          (const :tag "Insert default value.  Leave cursor at beginning"           insert-start)
          (const :tag "Insert default value.  Leave cursor at end"                 insert-end)
          (const :tag "Insert default value, select it, leave cursor at beginning"
           preselect-start)
          (const :tag "Insert default value, select it, leave cursor at end"
           preselect-end))
  :group 'Icicles-Miscellaneous)

;;;###autoload
(defcustom icicle-define-alias-commands-flag t
  "*Non-nil means define some commands that do not begin with `icicle-'.
For convenience, a few top-level commands are defined, typically as
aliases for commands with longer names.  For example, command `toggle'
is defined as an alias for command `icicle-toggle-option'.  In any
case, no such command is ever defined by Icicles if a function with
the same name is already defined."
   :type 'boolean :group 'Icicles-Miscellaneous)

;;;###autoload
(defcustom icicle-deletion-action-flag t
  "*Non-nil means `S-delete' during completion deletes the current object.
More precisely, it deletes the object named by the current completion
candidate, if a deletion action is defined for the current command.
If no deletion action is defined, then the value of this option has no
effect.

If you are worried about inadvertently deleting an object by
accidentally hitting `S-delete', you can customize this to nil to
inhibit `S-delete' object deletion during completion."
  :type 'boolean :group 'Icicles-Miscellaneous)

;;;###autoload
(defcustom icicle-dot-show-regexp-flag nil
  "*Non-nil means show `icicle-anychar-regexp' explicitly for `.'.
Otherwise, display it as a highlighted `.' only.
This has no effect for Emacs versions prior to 21: acts as if non-nil."
  :type 'boolean :group 'Icicles-Matching :group 'Icicles-Minibuffer-Display)

;;;###autoload
(defcustom icicle-dot-string "."
  "*String inserted by `icicle-insert-dot-command'.
It is either \".\" or the value of `icicle-anychar-regexp'.
You can toggle this at any time using command `icicle-toggle-dot',
bound to \\<minibuffer-local-completion-map>`\\[icicle-toggle-dot]' during completion."
  :set #'(lambda (sym defs)
           (custom-set-default sym defs)
           (setq icicle-dot-string-internal  icicle-dot-string))
  :type `(choice
          (const :tag "Match any char EXCEPT newline"       ".")
          (const :tag "Match any char, including NEWLINE"   ,icicle-anychar-regexp))
  :group 'Icicles-Matching :group 'Icicles-Minibuffer-Display)

;;;###autoload
(defcustom icicle-expand-input-to-common-match-flag t ; Toggle with `C-;'.
  "*Non-nil means `S-TAB' expands input, still matching all candidates.
The expansion replaces your input in the minibuffer.

Your expanded input is typically the longest substring common to all
completion candidates and that matches your (complete) input pattern.

If you want to edit your original input, use \\<minibuffer-local-completion-map>\
`\\[icicle-retrieve-previous-input]'.

For apropos completion, your input is, in general, a regexp.  Setting
this option to nil will let you always work with a regexp in the
minibuffer for apropos completion - your regexp is then never replaced
by the expanded common match.

You can toggle this option at any time from the minibuffer using
`C-;'."
  :type 'boolean :group 'Icicles-Matching)

;;;###autoload
(defcustom icicle-file-extras nil
  "*List of additional file-name candidates added to the normal list.
List elements are strings."
  :type '(repeat string) :group 'Icicles-Files :group 'Icicles-Matching)

;;;###autoload
(defcustom icicle-file-match-regexp nil
  "*nil or a regexp that file-name completion candidates must match.
If nil, then this does nothing.  If a regexp, then show only
candidates that match it (and match the user input).
See also `icicle-file-no-match-regexp'."
  :type '(choice (const :tag "None" nil) regexp)
  :group 'Icicles-Files :group 'Icicles-Matching)

;;;###autoload
(defcustom icicle-file-no-match-regexp nil
  "*nil or a regexp that file-name completion candidates must not match.
If nil, then this does nothing.  If a regexp, then show only
candidates that do not match it.
See also `icicle-file-match-regexp'."
  :type '(choice (const :tag "None" nil) regexp)
  :group 'Icicles-Files :group 'Icicles-Matching)

;;;###autoload
(defcustom icicle-file-predicate nil
  "*nil or a predicate that file-name candidates must satisfy.
If nil, then this does nothing.  Otherwise, this is a function of one
argument, a candidate, and only candidates that satisfy the predicate
are displayed.  For example, this value will show only names of files
with more than 5000 bytes:

  (lambda (fil) (> (nth 5 (file-attributes file)) 5000))"
  :type '(choice (const :tag "None" nil) function)
  :group 'Icicles-Files :group 'Icicles-Matching)

;;;###autoload
(defcustom icicle-file-require-match-flag nil
  "*Override `icicle-require-match-flag' for file-name completion.
The possible values are as follows:
- nil means this option imposes nothing on completion;
  the REQUIRE-MATCH argument provided to the function governs behavior
- `no-match-required' means the same as a nil value for REQUIRE-MATCH
- `partial-match-ok' means the same as a t value for REQUIRE-MATCH
- `full-match-required' means the same as a non-nil, non-t value for
  REQUIRE-MATCH

Note: This option is provided mainly for use (binding) in
`icicle-define-command' and `icicle-define-file-command'.
You probably do not want to set this globally, but you can."
  :type '(choice
          (const :tag "Do not impose any match behavior"   nil)
          (const :tag "Do not require a match"             no-match-required)
          (const :tag "Require a partial match, with RET"  partial-match-ok)
          (const :tag "Require a full match"               full-match-required))
  :group 'Icicles-Files :group 'Icicles-Matching)

;;;###autoload
(defcustom icicle-file-sort nil
  "*A sort function for file names, or nil.
Examples of sort functions are `icicle-dirs-last-p' and
`icicle-last-modified-first-p'.  If nil, then file names are not
sorted."
  :type '(choice (const :tag "None" nil) function)
  :group 'Icicles-Files :group 'Icicles-Completions-Display)

;;;###autoload
(defcustom icicle-files-ido-like-flag nil
  "t means `icicle-file' and similar commands act more Ido-like.
Specifically, those commands then bind these options to t:
 `icicle-show-Completions-initially-flag'
 `icicle-top-level-when-sole-completion-flag'
 `icicle-default-value'"
  :type 'boolean
  :group 'Icicles-Files :group 'Icicles-Completions-Display :group 'Icicles-Matching)

;;;###autoload
(defcustom icicle-filesets-as-saved-completion-sets-flag t
  "*Non-nil means you can use filesets to save candidates persistently.
This means that you can save file-name candidates in a persistent
Icicles saved completion set (cache file) or in in an Emacs fileset.
It also means that an Icicles persistent completion set can contain
filesets, in addition to file names: any number of filesets, and
filesets of different type.  Available only for Emacs 22 and later,
and you must load library `filesets.el'."
  :type 'boolean :group 'Icicles-Matching)

;;;###autoload
(defcustom icicle-guess-commands-in-path nil
  "*Non-nil means all shell commands are available for completion.
This is used in Icicle mode whenever a shell-command is read.

If non-nil, then all executable files (or all files, if option
`shell-completion-execonly' is nil) in your search path are included
among the completion candidates, in addition to any commands that are
guessed as being appropriate for the target files (e.g. marked files
in Dired).

If non-nil and if option `icicle-shell-command-candidates-cache' is
nil, then the list of commands is computed once and cached as the
value of `icicle-shell-command-candidates-cache'.  The particular
non-nil value of `icicle-guess-commands-in-path' determines when the
cache is filled, as follows:

- If the value is `load', then the cache is filled when Icicles is
  first loaded, and it is saved persistently.

- If the value is `first-use', then the cache is filled when you first
  complete a shell command, and the computed list is not saved
  persistently.

If the value is not `load', then whenever you enter Icicle mode the
cache is emptied.

If your environment changes and you want to update the cached list,
you can use command `icicle-recompute-shell-command-candidates'.  With
a prefix argument, that command also saves the cache persistently."
  :type '(choice
          (const :tag "Do not add shell commands from search path"              nil)
          (const :tag "Compute shell commands from path when Icicles is loaded" load)
          (const :tag "Compute shell commands from path upon first use"         first-use))
  :group 'Icicles-Miscellaneous)

;;;###autoload
(defcustom icicle-help-in-mode-line-flag t
  "*Non-nil means show help in the mode-line for individual completions.
If buffer *Completions* is displayed, then use its mode-line.
Otherwise, use the mode-line of the current buffer.

The help is shown when you cycle among completion candidates and when
your input is completed (entirely) to a candidate.

Face `icicle-mode-line-help' is used for the help."
  :type 'boolean :group 'Icicles-Completions-Display :group 'Icicles-Miscellaneous)

;;;###autoload
(defcustom icicle-hide-common-match-in-Completions-flag nil
  "*Non-nil means hide the common match for your input in *Completions*.
The common match is elided using ellipsis (`...').
You can use `C-M-.' during completion to toggle this option.

 The common match used here is governed by option
`icicle-expand-input-to-common-match-flag'."
  :type 'boolean :group 'Icicles-Miscellaneous)

;;;###autoload
(defcustom icicle-highlight-historical-candidates-flag t ; Toggle with `C-pause'.
  "*Non-nil means highlight *Completions* candidates that have been used.
This is done using face `icicle-historical-candidate'.
Historical candidates are those that you have entered (using `RET' or
`S-RET') previously.  You can toggle this option from the minibuffer
at any time using `C-pause'."
  :type 'boolean :group 'Icicles-Completions-Display)

;;;###autoload
(defcustom icicle-highlight-input-completion-failure 'implicit-strict
  "*Non-nil means highlight the part of your input that does not complete.
This is done using face `icicle-input-completion-fail' or
`icicle-input-completion-fail-lax'.

You can use `\\<minibuffer-local-completion-map>\\[icicle-goto/kill-failed-input]' \
to go to the start of the highlighted part.
Repeat to kill it.

This highlighting can have a negative impact on performance, because
it can mean recomputing completion candidates multiple times, in order
to determine the longest part that completes.  For this reason, you
can fine tune when you want this highlighting to occur.  The values of
this option and options
`icicle-highlight-input-completion-failure-delay' and
`icicle-highlight-input-completion-failure-threshold' determine when
the highlighting can take place.

In particular, highlighting the non-matching part of remote file names
can be slow.  Two values of this option allow remote file name
highlighting: `always' and `explicit-remote'.  The other values do not
highlight remote file names.  You probably do not want to use a value
of `always'.

If the value is nil, then highlighting never occurs.  If the value is
`explicit-strict', `explicit', or `explicit-remote', then highlighting
occurs only upon demand: when you hit `TAB' or `S-TAB' to request
completion.  If the value is `implicit-strict', `implicit', or
`always', then highlighting occurs also when you update your input
during incremental completion.

If the value is `implicit-strict' or `implicit', then highlighting
occurs not only upon demand but also during incremental completion if
`icicle-incremental-completion-flag' is non-nil.  Remember that you
can toggle incremental completion, using `C-#' in the minibuffer.

I use a value of `implicit' myself, but the default value is
`implicit-strict' because, depending on your setup and use cases,
`implicit' can impact performance for file-name completion (which is
lax, not strict).  I suggest you try `implicit' to see - this feature
is especially useful for file names.

Summary of choices for when to highlight:

nil               Never
`explicit-strict' When you hit `TAB'/`S-TAB' for strict completion
`explicit'        When you hit `TAB'/`S-TAB'
`explicit-remote' When you hit `TAB'/`S-TAB', including remote files
`implicit-strict' During strict completion
`implicit'        During lax or strict completion
`always'          Always, even for names of remote files

After highlighting, you can use `C-M-l' to move the cursor to the
start of the mismatch, for editing there.  You can use a second
`C-M-l' to kill (delete) the mismatch up to the next input line (if
any).  You can repeat `C-M-l' to kill additional input lines.

See also:
* `icicle-highlight-input-completion-failure-delay'
* `icicle-highlight-input-completion-failure-threshold'"
  :type '(choice
          (const :tag "Never"                                               nil)
          (const :tag "Explicit (`TAB'/`S-TAB') strict completion"          explicit-strict)
          (const :tag "Explicit (`TAB'/`S-TAB') lax and strict completion"  explicit)
          (const :tag "Explicit completion, even of remote file names"      explicit-remote)
          (const :tag "Strict completion"                                   implicit-strict)
          (const :tag "Lax and strict completion"                           implicit)
          (const :tag "Always (including for remote file names)"            always))
  :group 'Icicles-Minibuffer-Display)

;;;###autoload
(defcustom icicle-highlight-input-completion-failure-delay 0.7
  "*Seconds to wait before highlighting non-completing part of your input.
Zero means there is no wait."
  :type 'number :group 'Icicles-Minibuffer-Display)

;;;###autoload
(defcustom icicle-highlight-input-completion-failure-threshold 1000
  "*More candidates means do not highlight non-completing part of input.
See also `icicle-highlight-input-completion-failure'."
  :type 'integer :group 'Icicles-Minibuffer-Display)

;;;###autoload
(defcustom icicle-highlight-input-initial-whitespace-flag t
  "*Non-nil means highlight initial whitespace in your input.
This is done using face `icicle-whitespace-highlight'.
Purpose: Otherwise, you might not notice that you accidentally typed
some whitespace at the beginning of your input, so you might not
understand the set of matching candidates (or lack thereof).

Note: Highlighting input completion failure (see option
`icicle-highlight-input-completion-failure') subsumes
initial-whitespace highlighting.  This means that if no completion
candidate starts with whitespace, and if Icicles is highlighting input
completion failure, then only that highlighting is shown."
  :type 'boolean :group 'Icicles-Minibuffer-Display)

;;;###autoload
(defcustom icicle-highlight-lighter-flag t
  "*Non-nil means highlight the `Icy' mode-line lighter during completion.
See the Icicles doc, section `Nutshell View of Icicles', subsection
`Completion Status Indicators' for more information."
  :type 'boolean :group 'Icicles-Miscellaneous)

;;;###autoload
(defcustom icicle-ignored-directories (and (boundp 'vc-directory-exclusion-list)
                                           vc-directory-exclusion-list)
  "Directories ignored by `icicle-locate-file'."
  :type '(repeat string) :group 'Icicles-Files)

;;;###autoload
(defcustom icicle-ignore-space-prefix-flag nil ; Toggle with `M-_'.
  "*Non-nil means ignore completion candidates that start with a space.
However, such candidates are not ignored for prefix completion when
the input also starts with a space.  You can toggle this option from
the minibuffer using `M-_'.
Note: Some Icicles functionalities ignore the value of this option."
  :type 'boolean :group 'Icicles-Matching)

;;;###autoload
(defcustom icicle-incremental-completion-delay 0.7
  "*Number of seconds to wait before updating *Completions* incrementally.
There is no wait if the number of completion candidates is less than
or equal to `icicle-incremental-completion-threshold'.
See also `icicle-incremental-completion-flag'."
  :type 'number :group 'Icicles-Completions-Display)

;;;###autoload
(defcustom icicle-incremental-completion-flag t ; Toggle with `C-#'.
  "*Non-nil means update *Completions* buffer incrementally, as you type.
nil means do not update *Completions* buffer incrementally, as you type.
t means do nothing if *Completions* is not already displayed.
Non-nil and non-t means display *Completions* and update it.
You can toggle this between t and nil from the minibuffer at any time
using `C-#'.

Note: Incremental completion is effectively turned off when a remote
file name is read, that is, whenever your file-name input matches a
remote-file syntax.

See also `icicle-incremental-completion-delay' and
`icicle-incremental-completion-threshold'."
  :type '(choice
          (const :tag "Do not update *Completions* incrementally"                nil)
          (const :tag "Update *Completions* incrementally if already displayed"  t)
          (other :tag "Update *Completions* incrementally always"                always))
  :group 'Icicles-Completions-Display)

;;;###autoload
(defcustom icicle-incremental-completion-threshold 1000
  "*More candidates means apply `icicle-incremental-completion-delay'.
See also `icicle-incremental-completion-flag' and
`icicle-incremental-completion-delay'.
This threshold is also used to decide when to display the message
 \"Displaying completion candidates...\"."
  :type 'integer :group 'Icicles-Completions-Display)

;;;###autoload
(defcustom icicle-inhibit-advice-functions
  '(choose-completion choose-completion-string completing-read
    completion-setup-function dired-smart-shell-command
    display-completion-list exit-minibuffer face-valid-attribute-values
    minibuffer-complete-and-exit mouse-choose-completion
    next-history-element read-face-name read-file-name read-number
    shell-command shell-command-on-region switch-to-completions
    completing-read-multiple)
  "Functions that Icicles redefines, and for which advice is deactivated.
Icicle mode deactivates all advice for such functions.  The advice is
reactivated when you leave Icicle mode."
  :type '(repeat (function :tag "Function for which Icicles deactivates advice"))
  :group 'Icicles-Miscellaneous)

;;;###autoload
(defcustom icicle-inhibit-ding-flag nil
  "*Non-nil means Icicles never uses an audible bell (ding).
If nil, Icicles sometimes signals you with a sound."
  :type 'boolean :group 'Icicles-Miscellaneous)

;;;###autoload
(defcustom icicle-input-string ".*"
  "*String to insert in minibuffer via `\\<minibuffer-local-completion-map>\
\\[icicle-insert-string-from-variable]'.
Typically, this is a regexp or a portion of a regexp."
  :type 'string :group 'Icicles-Miscellaneous)

(when (fboundp 'defvaralias)            ; Emacs 22+
  (defvaralias 'icicle-key-descriptions-use-angle-brackets-flag
      'icicle-key-descriptions-use-<>-flag))

;;;###autoload
(defcustom icicle-inter-candidates-min-spaces 1
  "*Minimum number of spaces between candidates displayed in *Completions*.
If you use Do Re Mi (library `doremi.el'), then you can modify this
option incrementally during completion, seeing the effect as it
changes.  Use `\\<minibuffer-local-completion-map>\
\\[icicle-doremi-inter-candidates-min-spaces+]' from the minibuffer, then use the `up' and
`down' arrow keys or the mouse wheel to increment and decrement the
value.  WYSIWYG.

See also option `icicle-candidate-width-factor' and (starting with
Emacs 23) option `icicle-Completions-text-scale-decrease'."
  :type 'integer :group 'Icicles-Completions-Display)

;;;###autoload
(defcustom icicle-isearch-complete-keys
  '([C-M-tab] [M-tab] "\M-\t" [escape tab] ; Replace vanilla completion.
    "\M-o")                             ; Like Icicles minibuffer `M-o'.
  "*Key sequences to use for `icicle-isearch-complete'.
A list of values that each has the same form as a key-sequence
argument to `define-key'.

The default value includes `M-TAB', which replaces the vanilla binding
of `isearch-complete'.

It also includes `ESC TAB' and `C-M-TAB', because some operating
systems intercept `M-TAB' for their own use.  (Note: For MS Windows,
you can use (w32-register-hot-key [M-tab]) to allow Emacs to use
`M-TAB'.)

It also includes `M-o', in keeping with the Icicles use of `M-o'
during minibuffer completion."
  :type '(repeat sexp) :group 'Icicles-Key-Bindings)

;;;###autoload
(defcustom icicle-key-complete-keys '([S-tab] [S-iso-lefttab])
  ;; $$$$$ The following should be sufficient, but some Emacs 22+ libraries, such as `info.el',
  ;; are brain-dead and explicitly bind both `backtab' and `S-tab'.  I filed Emacs bug #1281.
  ;;   (if (> emacs-major-version 21)
  ;;       '([backtab])
  ;;     '([S-tab] [S-iso-lefttab]))
  "*Key sequences to use for `icicle-complete-key'.
A list of values that each has the same form as a key-sequence
argument to `define-key'.  It is a list mainly in order to accommodate
different keyboards - for example, `S-tab' and `S-iso-lefttab'."
;; In Emacs 22 and later, `backtab' is the canonical key that represents
;; both `S-tab' and `S-iso-lefttab', so that is used in the default
;; value.  If, for some reason, `backtab' is not being translated to
;; `S-tab' and `S-iso-lefttab' on your platform, you might want to
;; customize the value to ([S-tab] [S-iso-lefttab]).  And if your Emacs
;; version is 22 or later, please file an Emacs bug about the lack of
;; translation.
  :type '(repeat sexp) :group 'Icicles-Key-Completion :group 'Icicles-Key-Bindings)

;;;###autoload
(defcustom icicle-key-descriptions-use-<>-flag nil
  "*Non-nil means Icicles key descriptions should use angle brackets (<>).
For example, non-nil gives `<mode-line>'; nil gives `mode-line'.

This does not affect Emacs key descriptions outside of
Icicles (e.g. `C-h k' or `C-h w').

This has no effect for versions of Emacs prior to 21, because
they never use angle brackets."
  :type 'boolean :group 'Icicles-Key-Completion :group 'Icicles-Minibuffer-Display)

;;;###autoload
(defcustom icicle-keymaps-for-key-completion
  '(bookmark-bmenu-mode-map bmkp-jump-map bmkp-jump-other-window-map
    calendar-mode-map dired-mode-map facemenu-keymap jde-mode-map jde-jdb-mode-map
    senator-mode-map srecode-mode-map synonyms-mode-map vc-dired-mode-map)
  "*List of keymaps in which to bind `S-TAB' to `icicle-complete-keys'.
List elements are symbols that are bound to keymaps.

Each keymap should have at least one prefix key.  `S-TAB' is bound in
each keymap, so that you can use it to complete the prefix keys.

If one of the keymaps is not defined when Icicle mode is entered, then
it is ignored.  If you later define it, then just exit and reenter
Icicle mode, to bind `S-TAB' in the newly defined map.  For example,
use `M-x icy-mode' twice after entering Calendar mode, to be able to
complete `calendar-mode' prefix keys such as `A'.

Do not add `global-map' or any keymaps, such as `ctl-x-map', that are
accessible from the global keymap to the list - they are already
treated, by default.

Do not add any of the translation keymaps, `function-key-map',
`key-translation-map', or `iso-transl-ctl-x-8-map' to the list - that
will not work."
  :type '(repeat symbol) :group 'Icicles-Key-Completion :group 'Icicles-Key-Bindings)

;;;###autoload
(when (boundp 'kmacro-ring)             ; Emacs 22+
  (defcustom icicle-kmacro-ring-max (if (boundp 'most-positive-fixnum)
                                        most-positive-fixnum
                                      67108863) ; 1/2 of `most-positive-fixnum' on Windows.
    "*Icicles version of `kmacro-ring-max'."
    :type 'integer :group 'Icicles-Miscellaneous))

;;;###autoload
(defcustom icicle-levenshtein-distance 1
  "*Levenshtein distance allowed for strings to be considered as matching.
Icicles matching function `icicle-levenshtein-match' considers a
string to match another if the first string is within this distance of
some substring of the second.
This option is used only if you have library `levenshtein.el'."
  :type 'integer :group 'Icicles-Matching)

;;;###autoload
(defcustom icicle-list-end-string "

"
  "*String appended to a completion candidate that is a list of strings.
When a completion candidate is a list of strings, they are joined
pairwise using `icicle-list-join-string', and `icicle-list-end-string'
is appended to the joined strings.  The result is what is displayed as
a completion candidate in buffer *Completions*, and that is what is
matched by your minibuffer input.

The purpose of `icicle-list-end-string' is to allow some separation
between the displayed completion candidates.  Candidates that are
provided to input-reading functions such as `completing-read' as lists
of strings are often displayed using multiple lines of text.  If
`icicle-list-end-string' is \"\", then the candidates appear run
together, with no visual separation.

It is important to remember that `icicle-list-end-string' is part of
each completion candidate in such circumstances.  This matters if you
use a regexp that ends in `$', matching the end of the candidate."
  :type 'string :group 'Icicles-Completions-Display)

;; Note: If your copy of this file does not have the two-character string "^G^J"
;; (Control-G, Control-J) or, equivalently, \007\012, as the default value, you will want
;; to change the file to have that.  To insert these control characters in the file, use
;; `C-q'.  Emacs Wiki loses the ^G from the file, so I use \007, which works OK.
;;
;;;###autoload
(defcustom icicle-list-join-string (let ((strg  (copy-sequence "\007\012")))
                                     ;; Emacs 20 ignores `display', so don't bother.
                                     ;; Emacs 21 has a big bug, which interprets `display' badly.
                                     (when (> emacs-major-version 21) ; Avoid Emacs 21 bug.
                                       (set-text-properties 0 1 '(display "") strg))
                                     strg)
  "*String joining items in a completion that is a list of strings.
When a completion candidate is a list of strings, this string is used
to join the strings in the list, for display and matching purposes.
When completing input, you type regexps that match the strings,
separating them pairwise by the value of `icicle-list-join-string'.
Actually, what you enter is interpreted as a single regexp to be
matched against the joined strings.  Typically, the candidate list
contains two strings: a name and its doc string.

A good value for this option is a string that:
 1) does not normally occur in doc strings,
 2) visually separates the two strings it joins, and
 3) is not too difficult or too long to type.

The default value is \"^G\^J\", that is, control-g followed by
control-j (newline):
 1) ^G does not normally occur in doc strings
 2) a newline visually separates the multiple component strings, which
    helps readability in buffer *Completions*
 3) you can type the value using `C-q C-g C-q C-j'.

For readability (in Emacs 22 and later), the default value has a
`display' property that makes it appear as simply a newline in
*Completions* - the `^G' is hidden.  you can also make the default
value appear this way in your minibuffer input also, by using \
`\\<minibuffer-local-completion-map>\\[icicle-insert-list-join-string].'

If you like the default value of `^G^J', but you prefer that the `^G'
not be hidden, then just customize this option.  In Customize, use
`Show initial Lisp expression' after clicking the `State' button, to
be able to edit the default value.  Remove the `set-text-properties'
expression, which sets text property `display' to \"\"."
  :type 'string :group 'Icicles-Completions-Display)

;;;###autoload
(defcustom icicle-list-nth-parts-join-string " "
  "*String joining candidate parts split by `icicle-list-use-nth-parts'.
This has an effect on multi-completion candidates only, and only if
the current command uses `icicle-list-use-nth-parts'."
  :type 'string :group 'Icicles-Completions-Display)

;;;###autoload
(defcustom icicle-max-candidates nil
  "Non-nil means truncate completion candidates to at most this many.
If you use library `doremi.el' then you can use `C-x #' during
completion to increment or decrement the option value using the
vertical arrow keys or the mouse wheel.  A numeric prefix argument for
`C-x #' sets the increment size.  A plain prefix argument (`C-u')
resets `icicle-max-candidates' to nil, meaning no truncation."
  :type '(choice (const :tag "None" nil) integer)
  :group 'Icicles-Completions-Display :group 'Icicles-Matching
  :group 'Icicles-Buffers :group 'Icicles-Files)

;;;###autoload
(defcustom icicle-menu-items-to-history-flag t
  "Non-nil means to add menu-item commands to the command history.
This history is `extended-command-history'.

After you change the value of this option, toggle Icicle mode off,
then on again, for the change to take effect in the same session."
  :type 'boolean :group 'Icicles-Miscellaneous)

;; Inspired from `icomplete-minibuffer-setup-hook'.
;;;###autoload
(defcustom icicle-minibuffer-setup-hook nil
  "*Functions run at the end of minibuffer setup for Icicle mode."
  :type 'hook :group 'Icicles-Miscellaneous)

;;;###autoload
(defcustom icicle-modal-cycle-down-keys '([down])
  "*Key sequences to use for modal cycling to the next candidate.
A list of values that each has the same form as a key-sequence
argument to `define-key'.  It is a list mainly in order to accommodate
different keyboards.

This is used only if `icicle-cycling-respects-completion-mode' is
non-nil.

Option `icicle-use-C-for-actions-flag' swaps these keys with
`icicle-modal-cycle-down-action-keys'."
  :type '(repeat sexp) :group 'Icicles-Key-Bindings)

;;;###autoload
(defcustom icicle-modal-cycle-down-action-keys '([C-down])
  "*Keys for modal completion to cycle next and perform action.
A list of values that each has the same form as a key-sequence
argument to `define-key'.  It is a list mainly in order to accommodate
different keyboards.

This is used only if `icicle-cycling-respects-completion-mode' is
non-nil.

Option `icicle-use-C-for-actions-flag' swaps these keys with
`icicle-modal-cycle-down-keys'."
  :type '(repeat sexp) :group 'Icicles-Key-Bindings)

;;;###autoload
(defcustom icicle-modal-cycle-down-alt-action-keys '([C-S-down])
  "*Keys for modal completion to cycle next and perform alt action.
A list of values that each has the same form as a key-sequence
argument to `define-key'.  It is a list mainly in order to accommodate
different keyboards.

This is used only if `icicle-cycling-respects-completion-mode' is
non-nil."
  :type '(repeat sexp) :group 'Icicles-Key-Bindings)

;;;###autoload
(defcustom icicle-modal-cycle-down-help-keys '([(control meta down)]) ; `C-M-down'
  "*Keys for modal completion to cycle next and show candidate help.
A list of values that each has the same form as a key-sequence
argument to `define-key'.  It is a list mainly in order to accommodate
different keyboards.

This is used only if `icicle-cycling-respects-completion-mode' is
non-nil."
  :type '(repeat sexp) :group 'Icicles-Key-Bindings)

;;;###autoload
(defcustom icicle-modal-cycle-up-keys '([up])
  "*Key sequences to use for modal cycling to the previous candidate.
A list of values that each has the same form as a key-sequence
argument to `define-key'.  It is a list mainly in order to accommodate
different keyboards.

This is used only if `icicle-cycling-respects-completion-mode' is
non-nil.

Option `icicle-use-C-for-actions-flag' swaps these keys with
`icicle-modal-cycle-up-action-keys'."
  :type '(repeat sexp) :group 'Icicles-Key-Bindings)

;;;###autoload
(defcustom icicle-modal-cycle-up-action-keys '([C-up])
  "*Keys for modal completion to cycle previous and perform action.
A list of values that each has the same form as a key-sequence
argument to `define-key'.  It is a list mainly in order to accommodate
different keyboards.

This is used only if `icicle-cycling-respects-completion-mode' is
non-nil.

Option `icicle-use-C-for-actions-flag' swaps these keys with
`icicle-modal-cycle-up-keys'."
  :type '(repeat sexp) :group 'Icicles-Key-Bindings)

;;;###autoload
(defcustom icicle-modal-cycle-up-alt-action-keys '([C-S-up])
  "*Keys for modal completion to cycle previous and perform alt action.
A list of values that each has the same form as a key-sequence
argument to `define-key'.  It is a list mainly in order to accommodate
different keyboards.

This is used only if `icicle-cycling-respects-completion-mode' is
non-nil."
  :type '(repeat sexp) :group 'Icicles-Key-Bindings)

;;;###autoload
(defcustom icicle-modal-cycle-up-help-keys '([(control meta up)]) ; `C-M-up'
  "*Keys for modal completion to cycle previous and show candidate help.
A list of values that each has the same form as a key-sequence
argument to `define-key'.  It is a list mainly in order to accommodate
different keyboards.

This is used only if `icicle-cycling-respects-completion-mode' is
non-nil."
  :type '(repeat sexp) :group 'Icicles-Key-Bindings)

;;;###autoload
(defcustom icicle-no-match-hook nil
  "*List of hook functions run during completion when there are no matches."
  :type 'hook :group 'Icicles-Miscellaneous)

;;;###autoload
(defcustom icicle-option-type-prefix-arg-list '(direct inherit inherit-or-value direct-or-value
                                                inherit-or-regexp direct-or-regexp)
  "*Symbols controlling prefix args for `icicle-describe-option-of-type'.
A list of six symbols taken from this list:

  direct            inherit             inherit-or-value
  direct-or-value   inherit-or-regexp   direct-or-regexp

Choose the order you like.  The list members map, in order left to
right, to these prefix argument keys:

 `C-u C-u'           `C-0'            `C-u'
 `C-9' (positive)    no prefix arg    `C--' (negative)

For the meanings of the symbols, see the doc string of
`icicle-describe-option-of-type', which describes the default
prefix-argument bindings for the command."
  :type '(list symbol symbol symbol symbol symbol symbol) :group 'Icicles-Key-Bindings)

;;;###autoload
(when (> emacs-major-version 22)
  (defcustom icicle-populate-interactive-history-flag nil
    "*Non-nil means populate `icicle-interactive-history'.
That means add commands invoked interactively to that history, for use
during completion by `C-M-pause'.

After you change the value of this option, toggle Icicle mode off,
then on again, for the change to take effect in the same session.

Be aware that this history can become quite long.

Furthermore, there is an Emacs bug (#3984) that causes interactiveness
tests (`interactive-p' and `called-interactively-p') to fail, whenever
`call-interactively' is advised (which is how Icicles implements this
feature)."
    :type 'boolean :group 'Icicles-Miscellaneous))

;;;###autoload
(defcustom icicle-pp-eval-expression-print-length nil
  "*Value for `print-length' while printing value in `pp-eval-expression'.
A value of nil means no limit."
  :type '(choice (const :tag "No Limit" nil) integer) :group 'Icicles-Miscellaneous)

;;;###autoload
(defcustom icicle-pp-eval-expression-print-level nil
  "*Value for `print-level' while printing value in `pp-eval-expression'.
A value of nil means no limit."
  :type '(choice (const :tag "No Limit" nil) integer) :group 'Icicles-Miscellaneous)

;;;###autoload
(defcustom icicle-prefix-complete-keys '([tab] [(control ?i)]) ; `C-i' is `TAB'.
  "*Key sequences to use for `icicle-prefix-complete'.
A list of values that each has the same form as a key-sequence
argument to `define-key'.  It is a list mainly in order to accommodate
different keyboards."
  :type '(repeat sexp) :group 'Icicles-Key-Bindings)

;;;###autoload
(defcustom icicle-prefix-complete-no-display-keys '([(control meta tab)])
  "*Key sequences to use for `icicle-prefix-complete-no-display'.
A list of values that each has the same form as a key-sequence
argument to `define-key'.  It is a list mainly in order to accommodate
different keyboards."
  :type '(repeat sexp) :group 'Icicles-Key-Bindings)

;;;###autoload
(defcustom icicle-prefix-cycle-next-keys '([down])
  "*Key sequences for prefix completion to cycle to the next candidate.
This is also used to move down a line in the *Completions* buffer.
A list of values that each has the same form as a key-sequence
argument to `define-key'.  It is a list mainly in order to accommodate
different keyboards.

Option `icicle-use-C-for-actions-flag' swaps these keys with
`icicle-prefix-cycle-next-action-keys'."
  :type '(repeat sexp) :group 'Icicles-Key-Bindings)

;;;###autoload
(defcustom icicle-prefix-cycle-next-action-keys '([C-down])
  "*Keys for prefix completion to cycle next and perform action.
A list of values that each has the same form as a key-sequence
argument to `define-key'.  It is a list mainly in order to accommodate
different keyboards.

Option `icicle-use-C-for-actions-flag' swaps these keys with
`icicle-prefix-cycle-next-keys'."
  :type '(repeat sexp) :group 'Icicles-Key-Bindings)

;;;###autoload
(defcustom icicle-prefix-cycle-next-alt-action-keys '([C-S-down])
  "*Keys for prefix completion to cycle next and perform alt action.
A list of values that each has the same form as a key-sequence
argument to `define-key'.  It is a list mainly in order to accommodate
different keyboards."
  :type '(repeat sexp) :group 'Icicles-Key-Bindings)

;;;###autoload
(defcustom icicle-prefix-cycle-next-help-keys '([(control meta down)]) ; `C-M-down'
  "*Keys for prefix completion to cycle next and show candidate help.
A list of values that each has the same form as a key-sequence
argument to `define-key'.  It is a list mainly in order to accommodate
different keyboards."
  :type '(repeat sexp) :group 'Icicles-Key-Bindings)

;;;###autoload
(defcustom icicle-prefix-cycle-previous-keys '([up])
  "*Key sequences for prefix completion to cycle to the previous candidate.
This is also used to move up a line in the *Completions* buffer.
A list of values that each has the same form as a key-sequence
argument to `define-key'.  It is a list mainly in order to accommodate
different keyboards.

Option `icicle-use-C-for-actions-flag' swaps these keys with
`icicle-prefix-cycle-previous-action-keys'."
  :type '(repeat sexp) :group 'Icicles-Key-Bindings)

;;;###autoload
(defcustom icicle-prefix-cycle-previous-action-keys '([C-up])
  "*Keys for prefix completion to cycle previous and perform action.
A list of values that each has the same form as a key-sequence
argument to `define-key'.  It is a list mainly in order to accommodate
different keyboards.

Option `icicle-use-C-for-actions-flag' swaps these keys with
`icicle-prefix-cycle-previous-keys'."
  :type '(repeat sexp) :group 'Icicles-Key-Bindings)

;;;###autoload
(defcustom icicle-prefix-cycle-previous-alt-action-keys '([C-S-up])
  "*Keys for prefix completion to cycle previous and perform alt action.
A list of values that each has the same form as a key-sequence
argument to `define-key'.  It is a list mainly in order to accommodate
different keyboards."
  :type '(repeat sexp) :group 'Icicles-Key-Bindings)

;;;###autoload
(defcustom icicle-prefix-cycle-previous-help-keys '([(control meta up)]) ; `C-M-up'
  "*Keys for prefix completion to cycle previous and show candidate help.
A list of values that each has the same form as a key-sequence
argument to `define-key'.  It is a list mainly in order to accommodate
different keyboards."
  :type '(repeat sexp) :group 'Icicles-Key-Bindings)

;;;###autoload
(defcustom icicle-previous-candidate-keys '([S-tab] [S-iso-lefttab])
  ;; $$$$$ The following should be sufficient, but some Emacs 22+ libraries, such as `info.el',
  ;; are brain-dead and explicitly bind both `backtab' and `S-tab'.  I filed Emacs bug #1281.
  ;;   (if (> emacs-major-version 21)
  ;;       '([backtab])
  ;;     '([S-tab] [S-iso-lefttab]))
  "*Key sequences to use for `icicle-move-to-previous-completion'.
In buffer *Completions*, this moves backward among candidates.

A list of values that each has the same form as a key-sequence
argument to `define-key'.  It is a list mainly in order to accommodate
different keyboards - for example, `S-tab' and `S-iso-lefttab'."
;; In Emacs 22 and later, `backtab' is the canonical key that represents
;; both `S-tab' and `S-iso-lefttab', so that is used in the default
;; value.  If, for some reason, `backtab' is not being translated to
;; `S-tab' and `S-iso-lefttab' on your platform, you might want to
;; customize the value to ([S-tab] [S-iso-lefttab]).  And if your Emacs
;; version is 22 or later, please file an Emacs bug about the lack of
;; translation.
  :type '(repeat sexp) :group 'Icicles-Key-Bindings)

;;;###autoload
(defcustom icicle-quote-shell-file-name-flag t
  "Non-nil means to double-quote the file name that starts a shell command.
This is used by `icicle-read-shell-command-completing'.

If this is nil, then Emacs commands such as `M-!' will not quote a
shell-command file name such as `c:/Program Files/My Dir/mycmd.exe'.
In that case, a shell such as `bash' fails for a shell command such as
`c:/Program Files/My Dir/mycmd.exe arg1 arg2 &', because it interprets
only `c:/Program' as the shell command.  That is, it interprets the
space characters in the file name as separators.  If this is non-nil,
then input such as `c:/Program Files/My Dir/mycmd.exe arg1 arg2 &' is
passed to the shell as
`\"c:/Program Files/My Dir/mycmd.exe\" arg1 arg2 &'.

See the doc string of `icicle-quote-file-name-part-of-cmd' for
information about the characters that, like SPC, lead to quoting."
  :type 'boolean :group 'Icicles-Miscellaneous)

;;;###autoload
(defcustom icicle-read+insert-file-name-keys '([(control meta shift ?f)])
  "*Key sequences to invoke `icicle-read+insert-file-name'.
A list of values that each has the same form as a key-sequence
argument to `define-key'.  It is a list mainly in order to accommodate
different keyboards."
  :type '(repeat sexp) :group 'Icicles-Key-Bindings)

;;;###autoload
(defcustom icicle-redefine-standard-commands-flag t
  "*Non-nil means Icicle mode redefines some standard Emacs commands."
  :type 'boolean :group 'Icicles-Miscellaneous)

;;;###autoload
(defcustom icicle-regexp-quote-flag nil ; Toggle with `C-`'.
  "*Non-nil means special characters in regexps are escaped.
This means that no characters are recognized as special: they match
themselves.  This turns apropos completion into simple substring
completion.  It also turns Icicles searching into literal searching.
You can toggle this option from the minibuffer at any
time using `C-`'."
  :type 'boolean :group 'Icicles-Matching)

;;;###autoload
(defcustom icicle-regexp-search-ring-max (if (boundp 'most-positive-fixnum)
                                             (/ most-positive-fixnum 10)
                                           13421772) ; 1/10 of `most-positive-fixnum' on Windows.
  "*Icicles version of `regexp-search-ring-max'."
  :type 'integer :group 'Icicles-Searching)

;; This is essentially a version of `doremi-increment-color-component' for value only.
;; Must be before `icicle-region-background'.
(defun icicle-increment-color-value (color increment)
  "Increase value component (brightness) of COLOR by INCREMENT."
  (unless (featurep 'hexrgb)
    (error "`icicle-increment-color-value' requires library `hexrgb.el'"))
  (unless (string-match "#" color)      ; Convert color name to #hhh...
    (setq color  (hexrgb-color-values-to-hex (x-color-values color))))
  ;; Convert RGB to HSV
  (let* ((rgb         (x-color-values color))
         (red         (/ (float (nth 0 rgb)) 65535.0)) ; Convert from 0-65535 to 0.0-1.0
         (green       (/ (float (nth 1 rgb)) 65535.0))
         (blue        (/ (float (nth 2 rgb)) 65535.0))
         (hsv         (hexrgb-rgb-to-hsv red green blue))
         (hue         (nth 0 hsv))
         (saturation  (nth 1 hsv))
         (value       (nth 2 hsv)))
    (setq value  (+ value (/ increment 100.0)))
    (when (> value 1.0) (setq value  (1- value)))
    (hexrgb-color-values-to-hex (mapcar (lambda (x) (floor (* x 65535.0)))
                                        (hexrgb-hsv-to-rgb hue saturation value)))))

;; You can use `icicle-increment-color-value' in place of `icicle-increment-color-hue', if you
;; prefer highlighting background to be slightly darker instead of a slightly different hue.
;;
;;;###autoload
(defcustom icicle-region-background
  (if (featurep 'hexrgb)
      (let* ((bg   (or (and (boundp '1on1-active-minibuffer-frame-background)
                            1on1-active-minibuffer-frame-background) ; In `oneonone.el'.
                       (let ((frame-bg  (cdr (assq 'background-color (frame-parameters)))))
                         (when (member frame-bg '(nil unspecified "unspecified-bg"))
                           (setq frame-bg (if (eq frame-background-mode 'dark) "Black" "White")))
                         (and frame-bg (x-color-defined-p frame-bg) frame-bg))
                       (face-background 'region)))
             (sat  (condition-case nil (hexrgb-saturation bg) (error nil))))
        (if sat
            (if (hexrgb-approx-equal sat 0.0)
                (icicle-increment-color-value bg ; Grayscale - change bg value slightly.
                                              (if (eq frame-background-mode 'dark) 20 -10))
              (icicle-increment-color-hue bg 24)) ; Color - change bg hue slightly.
          (face-background 'region)))
    (face-background 'region))          ; Use normal region background.
  "*Background color to use for the region during minibuffer cycling.
This has no effect if `icicle-change-region-background-flag' is nil.
If you do not define this explicitly, and if you have loaded library
`hexrgb.el' (recommended), then this color will be slightly
different from your frame background.  This still lets you notice the
region, but it makes the region less conspicuous, so you can more
easily read your minibuffer input."
  :type (if (and (require 'wid-edit nil t) (get 'color 'widget-type)) 'color 'string)
  :group 'Icicles-Minibuffer-Display)

;;;###autoload
(defcustom icicle-require-match-flag nil
  "*Control REQUIRE-MATCH arg to `completing-read' and `read-file-name'.
The possible values are as follows:
- nil means this option imposes nothing on completion;
  the REQUIRE-MATCH argument provided to the function governs behavior
- `no-match-required' means the same as a nil value for REQUIRE-MATCH
- `partial-match-ok' means the same as a t value for REQUIRE-MATCH
- `full-match-required' means the same as a non-nil, non-t value for
  REQUIRE-MATCH

Note: This option is provided mainly for use (binding) in
`icicle-define-command' and `icicle-define-file-command'.
You probably do not want to set this globally, but you can."
  :type '(choice
          (const :tag "Do not impose any match behavior"   nil)
          (const :tag "Do not require a match"             no-match-required)
          (const :tag "Require a partial match, with RET"  partial-match-ok)
          (const :tag "Require a full match"               full-match-required))
  :group 'Icicles-Matching)

;;;###autoload
(defcustom icicle-saved-completion-sets nil
  "*Completion sets available for `icicle-candidate-set-retrieve'.
The form is ((SET-NAME . CACHE-FILE-NAME)...), where SET-NAME is the
name of a set of completion candidates and CACHE-FILE-NAME is the
absolute name of the cache file that contains those candidates.
You normally do not customize this directly, statically.
Instead, you add or remove sets using commands
`icicle-add/update-saved-completion-set' and
`icicle-remove-saved-completion-set'."
  :type '(repeat (cons string file)) :group 'Icicles-Matching)

;;;###autoload
(defcustom icicle-search-cleanup-flag t
  "*Controls whether to remove highlighting after a search.
If this is nil, highlighting can be removed manually with
`\\[icicle-search-highlight-cleanup]'."
  :type 'boolean :group 'Icicles-Searching)

;;;###autoload
(defcustom icicle-search-context-match-predicate nil
  "*nil or a predicate that candidate search contexts must satisfy.
If nil, then this does nothing.  Otherwise, this is a predicate of one
argument, a string, and only search contexts that satisfy it are
displayed.  Command `icicle-search' binds internal variable
`icicle-must-pass-predicate' to this value.

Note: This predicate is different from the predicate used by \
`\\<minibuffer-local-completion-map>\\[icicle-narrow-candidates-with-predicate]'.
That predicate takes as argument a full search-context candidate,
which includes the context position."
  :type '(choice (const :tag "None" nil) function) :group 'Icicles-Matching)

;;;###autoload
(defcustom icicle-search-from-isearch-keys '([S-tab] [S-iso-lefttab])
  ;; $$$$$ The following should be sufficient, but some Emacs 22+ libraries, such as `info.el',
  ;; are brain-dead and explicitly bind both `backtab' and `S-tab'.  I filed Emacs bug #1281.
  ;;   (if (> emacs-major-version 21)
  ;;       '([backtab])
  ;;     '([S-tab] [S-iso-lefttab]))
  "*Key sequences to use to start `icicle-search' from Isearch.
A list of values that each has the same form as a key-sequence
argument to `define-key'.  It is a list mainly in order to accommodate
different keyboards - for example, `S-tab' and `S-iso-lefttab'."
;; In Emacs 22 and later, `backtab' is the canonical key that represents
;; both `S-tab' and `S-iso-lefttab', so that is used in the default
;; value.  If, for some reason, `backtab' is not being translated to
;; `S-tab' and `S-iso-lefttab' on your platform, you might want to
;; customize the value to ([S-tab] [S-iso-lefttab]).  And if your Emacs
;; version is 22 or later, please file an Emacs bug about the lack of
;; translation.
  :type '(repeat sexp) :group 'Icicles-Key-Bindings)

;;;###autoload
(defcustom icicle-search-highlight-all-current-flag nil ; Toggle with `C-^'.
  "*Non-nil means highlight input match in each context search hit.
Setting this to non-nil can impact performance negatively, because the
highlighting is updated with each input change.  You can toggle this
option from the minibuffer during `C-c`' search using `C-^'."
  :type 'boolean :group 'Icicles-Searching)

;;;###autoload
(defcustom icicle-search-highlight-context-levels-flag t
  "*Non-nil means highlight 1-8 context levels, within the search context.
Level highlighting is done only when this is non-nil and a subgroup is
not used as the search context, that is, the context corresponds to
the entire search regexp."
  :type 'boolean :group 'Icicles-Searching)

;;;###autoload
(defcustom icicle-search-highlight-threshold 100000
  "*Max number of context search hits to highlight at once.
This highlighting uses face `icicle-search-main-regexp-others'."
  :type 'integer :group 'Icicles-Searching)

;;;###autoload
(defcustom icicle-search-hook nil
  "*List of hook functions run by `icicle-search' (see `run-hooks')."
  :type 'hook :group 'Icicles-Searching)

;;;###autoload
(defcustom icicle-search-replace-common-match-flag t ; Toggle with `M-;'.
  "*Non-nil means to replace the expanded common match of your input.
This has no effect if either
`icicle-search-highlight-all-current-flag' or
`icicle-expand-input-to-common-match-flag' is nil.
You can toggle those options from the minibuffer using `C-^' and
`C-|', respectively.  You can toggle
`icicle-search-replace-common-match-flag' using `M-;'."
  :type 'boolean :group 'Icicles-Searching)

;;;###autoload
(defcustom icicle-search-replace-literally-flag nil ; Toggle with `M-`'.
  "*Non-nil means to treat replacement text literally.
Otherwise (nil), interpret `\\' specially in replacement text, as in
the LITERAL argument to `replace-match'.

You can use `M-`' to toggle this at any time during Icicles search."
  :type 'boolean :group 'Icicles-Searching)

;;;###autoload
(defcustom icicle-search-replace-whole-candidate-flag t ; Toggle with `C-,'.
  "*Non-nil means replacement during search replaces the entire search hit.
Otherwise (nil), replace only what matches your current input.

You can use `C-,' to toggle this at any time during Icicles search."
  :type 'boolean :group 'Icicles-Searching)

;;;###autoload
(defcustom icicle-search-ring-max (if (boundp 'most-positive-fixnum)
                                             (/ most-positive-fixnum 10)
                                           13421772) ; 1/10 of `most-positive-fixnum' on Windows.
  "*Icicles version of `search-ring-max'."
  :type 'integer :group 'Icicles-Searching)

;;;###autoload
(defcustom icicle-search-whole-word-flag nil ; Toggle with `M-q'.
  "*Non-nil means that `icicle-search' looks for a whole word.
You can use `M-q' to toggle this at any time during Icicles search."
  :type 'boolean :group 'Icicles-Searching)

;; Based more or less on `shell-dynamic-complete-as-command'.
;; Must be before `icicle-shell-command-candidates-cache'.
(defun icicle-compute-shell-command-candidates ()
  "Compute shell command candidates from search path, and return them.
The candidates are the executable files in your search path or, if
`shell-completion-execonly' is nil, all files in your search path."
  (require 'shell)                      ; `shell-completion-execonly'
  (message "Finding commands in search path...")
  (let* ((filenondir         "")
         (path-dirs          (cdr (reverse exec-path)))
         (cwd                (file-name-as-directory (expand-file-name default-directory)))
         (ignored-extensions (and comint-completion-fignore
                                  (mapconcat #'(lambda (x) (concat (regexp-quote x) "$"))
                                             comint-completion-fignore "\\|")))
         (dir                "")
         (comps-in-dir       ())
         (file               "")
         (abs-file-name      "")
         (completions        ()))
    ;; Go through each dir in the search path, finding completions.
    (while path-dirs
      (setq dir           (file-name-as-directory (comint-directory (or (car path-dirs) ".")))
            comps-in-dir  (and (file-accessible-directory-p dir)
                               (file-name-all-completions filenondir dir)))
      ;; Go  see whether it should be used.
      (while comps-in-dir
        (setq file           (car comps-in-dir)
              abs-file-name  (concat dir file))
        (when (and (not (member file completions))
                   (not (and ignored-extensions (string-match ignored-extensions file)))
                   (or (string-equal dir cwd) (not (file-directory-p abs-file-name)))
                   (or (null shell-completion-execonly) (file-executable-p abs-file-name)))
          (setq completions  (cons file completions)))
        (setq comps-in-dir  (cdr comps-in-dir)))
      (setq path-dirs  (cdr path-dirs)))
    completions))

;;;###autoload
(defcustom icicle-shell-command-candidates-cache (and (eq icicle-guess-commands-in-path 'load)
                                                      (icicle-compute-shell-command-candidates))
  "Cache for shell command candidates.
You typically do not need to customize this option.
It is an option mainly to persist its value.
See `icicle-guess-commands-in-path'."
  :type '(repeat sexp) :group 'Icicles-Miscellaneous)

;;;###autoload
(if (and (fboundp 'defvaralias) (boundp 'completion-show-help))
    (defvaralias 'icicle-show-Completions-help-flag 'completion-show-help)
  (defcustom icicle-show-Completions-help-flag t
    "*Non-nil means display help lines at the top of buffer *Completions*."
    :type 'boolean :group 'Icicles-Completions-Display))

;;;###autoload
(defcustom icicle-show-Completions-initially-flag nil
  "*Non-nil means to show buffer *Completions* even without user input.
nil means that *Completions* is shown upon demand, via `TAB' or
`S-TAB'.

For an alternative but similar behavior to using non-nil for
`icicle-show-Completions-initially-flag', you can set option
`icicle-incremental-completion-flag' to a value that is neither nil
nor t.  That displays buffer *Completions* as soon as you type or
delete input, but not initially."
  :type 'boolean :group 'Icicles-Completions-Display)

;;;###autoload
(defcustom icicle-show-multi-completion-flag t
  "*Non-nil means to show completion candidates as multi-completions.
This has an effect only where multi-completion is available.
Also, some commands, such as `icicle-locate-file', use a prefix arg to
determine whether to show multi-completions.  Such commands generally
ignore this option.

A typical example of showing multi-completions is adding buffer names
to candidates to show which buffer they are associated with.  Some
commands, such as `icicle-search', append the name of the associated
buffer, highlighted, to the normal completion candidate.  This lets
you easily see which buffer the candidate applies to.  Also, the
buffer name is part of the candidate, so you can match against it.

Note: Even when the option value is nil, you can use `C-M-mouse-2' and
so on to see information about a candidate.  This information
typically includes whatever a non-nil value of the option would have
shown.

You can toggle this option from the minibuffer using `M-m'.  The new
value takes effect after you exit the minibuffer (i.e., for the next
command)."
  :type 'boolean :group 'Icicles-Completions-Display)

;; This is similar to `bmkp-sort-comparer'.
;;;###autoload
(defcustom icicle-sort-comparer 'icicle-case-string-less-p ; Cycle with `C-,'.
  "*Predicate or predicates for sorting (comparing) two items.
Used in particular to sort completion candidates.  In that case, this
determines the order of candidates when cycling and their order in
buffer *Completions*.

When `icicle-cycle-into-subdirs-flag' is non-nil, you might want to
use a function such as `icicle-dirs-last-p' for this option, to
prevent cycling into subdirectories depth first.  Command
`icicle-sort-by-directories-last' does that.

You can cycle completion sort orders at any time using `C-,' in the
minibuffer.

Although this is a user option, it may be changed by program
locally, for use in particular contexts.  In particular, you can bind
this to nil in an Emacs-Lisp function, to inhibit sorting in that
context.

Various sorting commands change the value of this option dynamically
\(but they do not save the changed value).

The value must be one of the following:

* nil, meaning do not sort

* a predicate that takes two items as args

* a list of the form ((PRED...) FINAL-PRED), where each PRED and
  FINAL-PRED are binary predicates

If the value is a list of predicates, then each PRED is tried in turn
until one returns a non-nil value.  In that case, the result is the
car of that value.  If no non-nil value is returned by any PRED, then
FINAL-PRED is used and its value is the result.

Each PRED should return `(t)' for true, `(nil)' for false, or nil for
undecided.  A nil value means that the next PRED decides (or
FINAL-PRED, if there is no next PRED).

Thus, a PRED is a special kind of predicate that indicates either a
boolean value (as a singleton list) or \"I cannot decide - let the
next guy else decide\".  (Essentially, each PRED is a hook function
that is run using `run-hook-with-args-until-success'.)

Examples:

 nil           - No sorting.

 string-lessp  - Single predicate that returns nil or non-nil.

 ((p1 p2))     - Two predicates `p1' and `p2', which each return
                 (t) for true, (nil) for false, or nil for undecided.

 ((p1 p2) string-lessp)
               - Same as previous, except if both `p1' and `p2' return
                 nil, then the return value of `string-lessp' is used.

Note that these two values are generally equivalent, in terms of their
effect (*):

 ((p1 p2))
 ((p1) p2-plain) where p2-plain is (icicle-make-plain-predicate p2)

Likewise, these three values generally act equivalently:

 ((p1))
 (() p1-plain)
 p1-plain        where p1-plain is (icicle-make-plain-predicate p1)

The PRED form lets you easily combine predicates: use `p1' unless it
cannot decide, in which case try `p2', and so on.  The value ((p2 p1))
tries the predicates in the opposite order: first `p2', then `p1' if
`p2' returns nil.

Using a single predicate or FINAL-PRED makes it easy to reuse an
existing predicate that returns nil or non-nil.

You can also convert a PRED-type predicate (which returns (t), (nil),
or nil) into an ordinary predicate, by using function
`icicle-make-plain-predicate'.  That lets you reuse elsewhere, as
ordinary predicates, any PRED-type predicates you define.

Note: As a convention, predefined Icicles PRED-type predicate names
have the suffix `-cp' (for \"component predicate\") instead of `-p'."
  ;; We don't bother to define a `icicle-reverse-multi-sort-order'
  ;; analogous to `bmkp-reverse-multi-sort-order'.  If we did, the doc
  ;; string would need to be updated to say what the doc string of
  ;; `bmkp-sort-comparer' says about `bmkp-reverse-multi-sort-order'.
  :type '(choice
          (const    :tag "None (do not sort)" nil)
          (function :tag "Sorting Predicate")
          (list     :tag "Sorting Multi-Predicate"
           (repeat (function :tag "Component Predicate"))
           (choice
            (const    :tag "None" nil)
            (function :tag "Final Predicate"))))
  :group 'Icicles-Matching :group 'Icicles-Completions-Display)

;;;###autoload
(defcustom icicle-buffer-configs
  `(("All" nil nil nil nil ,icicle-sort-comparer)
    ("Files" nil nil (lambda (bufname) (buffer-file-name (get-buffer bufname))) nil
     ,icicle-sort-comparer)
    ("Files and Scratch" nil nil (lambda (bufname) (buffer-file-name (get-buffer bufname)))
     ("*scratch*") ,icicle-sort-comparer)
    ("All, *...* Buffers Last" nil nil nil nil icicle-buffer-sort-*...*-last))
  "*List of option configurations available for `icicle-buffer-config'.
The form is (CONFIG...), where CONFIG is a list of these items:

 - Configuration name                    (string)
 - `icicle-buffer-match-regexp' value    (regexp string)
 - `icicle-buffer-no-match-regexp' value (regexp string)
 - `icicle-buffer-predicate' value       (function)
 - `icicle-buffer-extras' value          (list of strings)
 - `icicle-buffer-sort' value            (function)

A configuration describes which buffer names are displayed during
completion and their order."
  :type '(repeat (list
                  string                ; Configuration name
                  (choice (const :tag "None" nil) (string :tag "Match regexp"))
                  (choice (const :tag "None" nil) (string :tag "No-match regexp"))
                  (choice (const :tag "None" nil) (function :tag "Predicate")) ; Predicate
                  (choice (const :tag "None" nil) (repeat (string :tag "Extra buffer")))
                  (choice (const :tag "None" nil) (function :tag "Sort function"))))
  :group 'Icicles-Buffers)

(defun icicle-buffer-sort-*...*-last (buf1 buf2)
  "Return non-nil if BUF1 is `string<' BUF2 or only BUF2 starts with \"*\"."
  (let ((b1  (if completion-ignore-case (downcase buf1) buf1))
        (b2  (if completion-ignore-case (downcase buf2) buf2)))
    (if (string-match "^\\*" b1)
        (and (string-match "^\\*" b2) (string< b1 b2))
      (or (string-match "^\\*" b2) (string< b1 b2)))))

(when (> emacs-major-version 20)
  (defcustom icicle-sort-orders-alist ()
    "*Alist of available sort functions.
This is a pseudo option - you probably do NOT want to customize this.
Instead, use macro `icicle-define-sort-command' to define a new sort
function and automatically add it to this list.

Each alist element has the form (SORT-ORDER . COMPARER):

 SORT-ORDER is a short string or symbol describing the sort order.
 Examples: \"by date\", \"alphabetically\", \"directories first\".

 COMPARER compares two items.  It must be acceptable as a value of
 `icicle-sort-comparer'."
    :type '(alist
            :key-type (choice :tag "Sort order" string symbol)
            :value-type (choice
                         (const    :tag "None (do not sort)" nil)
                         (function :tag "Sorting Predicate")
                         (list     :tag "Sorting Multi-Predicate"
                          (repeat (function :tag "Component Predicate"))
                          (choice
                           (const    :tag "None" nil)
                           (function :tag "Final Predicate")))))
    :group 'Icicles-Completions-Display :group 'Icicles-Matching))

(unless (> emacs-major-version 20)      ; Emacs 20: custom type `alist' doesn't exist.
  (defcustom icicle-sort-orders-alist ()
    "*Alist of available sort functions.
This is a pseudo option - you probably do NOT want to customize this.
Instead, use macro `icicle-define-sort-command' to define a new sort
function and automatically add it to this list.

Each alist element has the form (SORT-ORDER . COMPARER):

 SORT-ORDER is a short string or symbol describing the sort order.
 Examples: \"by date\", \"alphabetically\", \"directories first\".

 COMPARER compares two items.  It must be acceptable as a value of
 `icicle-sort-comparer'."
    :type '(repeat
            (cons
             (choice :tag "Sort order" string symbol)
             (choice
              (const    :tag "None (do not sort)" nil)
              (function :tag "Sorting Predicate")
              (list     :tag "Sorting Multi-Predicate"
               (repeat (function :tag "Component Predicate"))
               (choice
                (const    :tag "None" nil)
                (function :tag "Final Predicate"))))))
    :group 'Icicles-Completions-Display :group 'Icicles-Matching))

;;;###autoload
(defcustom icicle-special-candidate-regexp nil
  "*Regexp to match special completion candidates, or nil to do nothing.
The candidates are highlighted in buffer *Completions* using face
`icicle-special-candidate'."
  :type '(choice (const :tag "None" nil) regexp) :group 'Icicles-Completions-Display)

;;;###autoload
(defcustom icicle-S-TAB-completion-methods-alist ; Cycle with `M-('.
  `(("apropos" . string-match)
    ("scatter" . icicle-scatter-match)
    ,@(and (require 'levenshtein nil t)
           '(("Levenshtein" . icicle-levenshtein-match)
             ("Levenshtein strict" . icicle-levenshtein-strict-match))))
  "*Alist of completion methods used by `S-TAB'.
Each element has the form (NAME . FUNCTION), where NAME is a string
name and FUNCTION is the completion match function.  NAME is used in
messages to indicate the type of completion matching.

By default, `S-TAB' is the key for this completion. The actual keys
used are the value of option `icicle-apropos-complete-keys'.

See also option `icicle-TAB-completion-methods'."
  :type '(alist
          :key-type   (string :tag "Name used in messages")
          :value-type (symbol :tag "Completion matching function"))
  :group 'Icicles-Matching)

;;;###autoload
(defcustom icicle-swank-prefix-length 1
  "*Length (chars) of symbol prefix that much match, for swank completion."
  :type 'integer :group 'Icicles-Matching)

;;;###autoload
(defcustom icicle-swank-timeout 3000
  "*Number of msec before swank (fuzzy symbol) completion gives up."
  :type 'integer :group 'Icicles-Matching)

;;;###autoload
(defcustom icicle-TAB-completion-methods ; Cycle with `C-('.
  (let ((methods  ()))
    (when (require 'el-swank-fuzzy nil t) (push 'swank   methods))
    (when (require 'fuzzy-match nil t)    (push 'fuzzy   methods))
    (when (boundp 'completion-styles)     (push 'vanilla methods))
    (push 'basic methods)
    methods)
  "*List of completion methods to use for \
`\\<minibuffer-local-completion-map>\\[icicle-prefix-complete]'.
The first method in the list is the default method.

The available methods can include these:

1. `basic'
2. `vanilla' (provided you have Emacs 23 or later)
3. `fuzzy'   (provided you have library `fuzzy-match.el')
4. `swank'   (provided you have library `el-swank-fuzzy.el')

1. Basic completion means ordinary prefix completion. It is the
`basic' completion style of Emacs 23 or later, and it is essentially
the completion style prior to Emacs 23 (Emacs 22 completion was
slightly different - see Emacs 23 option `custom-styles' for more
info).

2. Vanilla completion respects option `completion-styles' (new in
Emacs 23), so that `TAB' behaves similarly in Icicles to what it does
in vanilla Emacs.  The vanilla method also completes environment
variables during file-name completion and in shell commands.  The
non-vanilla methods do not complete environment variables, but the
variables are expanded to their values when you hit `RET'.

3. Fuzzy completion is a form of prefix completion in which matching
finds the candidates that have the most characters in common with your
input, in the same order, and with a minimum of non-matching
characters.  It can skip over non-matching characters, as long as the
number of characters skipped in the candidate is less that those
following them that match.  After the matching candidates are found,
they are sorted by skip length and then candidate length.

Fuzzy completion is described in detail in the commentary of library
`fuzzy-match.el'.  There is no fuzzy completion of file names - fuzzy
completion is the same as basic for file names.  Fuzzy completion is
always case-sensitive.

4. Swank completion in Icicles is the same as fuzzy completion, except
regarding symbols.  That is, swank completion per se applies only to
symbols.  Symbols are completed using the algorithm of library
`el-swank-fuzzy.el'.

Icicles options `icicle-swank-timeout' and
`icicle-swank-prefix-length' give you some control over the behavior.
When the `TAB' completion method is `swank', you can use `C-x 1'
\(`icicle-doremi-increment-swank-timeout+') and `C-x 2'
\(`icicle-doremi-increment-swank-prefix-length+') in the minibuffer to
increment these options on the fly using the arrow keys `up' and
`down'.

Swank symbol completion uses heuristics that relate to supposedly
typical patterns found in symbol names.  It also uses a timeout that
can limit the number of matches.  It is generally quite a bit slower
than fuzzy completion, and it sometimes does not provide all
candidates that you might think should match, even when all of your
input is a prefix (or even when it is already complete!).  If swank
completion produces no match when you think it should, remember that
you can use `\\[icicle-next-TAB-completion-method]' on the fly to \
change the completion method.


If you do not customize `icicle-TAB-completion-methods', then the
default value (that is, the available `TAB' completion methods) will
reflect your current Emacs version and whether you have loaded
libraries `fuzzy-match.el' and `el-swank-fuzzy.el'.

By default, `TAB' is the key for this completion. The actual keys
used are the value of option `icicle-prefix-complete-keys'.

See also option `icicle-S-TAB-completion-methods-alist'."
  :type (let ((methods  ()))
          (when (require 'el-swank-fuzzy nil t)
            (push '(const :tag "Swank (Fuzzy Symbol)" swank) methods))
          (when (require 'fuzzy-match nil t)
            (push '(const :tag "Fuzzy" fuzzy) methods))
          (when (boundp 'completion-styles)
            (push '(const :tag "Vanilla `completion-styles'" vanilla) methods))
          (push '(const :tag "Basic" basic) methods)
          `(repeat (choice ,@methods)))
  :group 'Icicles-Matching)

;;;###autoload
(defcustom icicle-TAB-shows-candidates-flag t
  "*Non-nil means that `TAB' always shows completion candidates.
Otherwise (nil), follow the standard Emacs behavior of completing to
the longest common prefix, and only displaying the candidates after a
second `TAB'.

Actually, the concerned keys are those defined by option
`icicle-prefix-complete-keys', not necessarily `TAB'."
  :type 'boolean :group 'Icicles-Key-Bindings)

;;;###autoload
(defcustom icicle-test-for-remote-files-flag t ; Toggle with `C-^'.
  "*Non-nil means Icicles tests for remote file names.
A value of nil turns off all handling of remote file names by Tramp,
including file-name completion.

The testing due to a non-nil value takes a little time, but the test
result saves time with Tramp handling, and it is used to avoid some
other costly operations when a file is determined to be remote.  These
operations are (a) incremental completion and (b) highlighting of the
part of your current input that does not complete.

Use a nil value only when you are sure that the file names you are
completing are local.  The effect will be a slight speed increase for
operations (a) and (b) for local files.

In addition, a nil value has the effect of ignoring the restriction of
input mismatch highlighting to strict completion.  That is, it treats
an `icicle-highlight-input-completion-failure' value of
`explicit-strict' or `implicit-strict' as if it were `implicit'.  The
assumption here is that you use these highlighting values only to
avoid the cost of remote file name completion.

You can toggle this option from the minibuffer using `C-^' (except
during Icicles search)."
  :initialize (lambda (opt-name val) (set opt-name t))
  :set #'(lambda (opt-name val)
           (or (not (require 'tramp nil t))
               (prog1 (set opt-name (not val))
                 (icicle-toggle-remote-file-testing))))
  :type 'boolean :group 'Icicles-Matching)

;;;###autoload
(defcustom icicle-thing-at-point-functions
  (progn (or (require 'ffap- nil t) (require 'ffap nil t)) ; Try `ffap-.el' first.
         (cons
          ;; 1) Lisp symbol or file name, 2) region-or-word, 3) Ffap, 4)url.
          `(,(if (fboundp 'symbol-name-nearest-point)
                 'symbol-name-nearest-point
                 (lambda () (symbol-name (symbol-at-point))))
            ,(if (fboundp 'region-or-word-nearest-point)
                 'region-or-word-nearest-point
                 (lambda () (thing-at-point 'word)))
            ,@(and (fboundp 'ffap-guesser) '(ffap-guesser))
            thing-at-point-url-at-point)
          'forward-word))
  "*Functions that return a string at or near the cursor when you use `M-.'.
A cons cell whose car and cdr may each be empty.

The car of the cons cell is a list of functions that grab different
kinds of strings at or near point.  By default, there are four
functions, which grab 1) whatever `ffap-guesser' finds, 2) the symbol
or file name, 3) the word, 4) the URL at point.  Any number of
functions can be used.  They are used in sequence by command
`icicle-insert-string-at-point' (bound to `M-.').

The cdr of the cons cell is nil or a function that advances point one
text thing.  Each time command `icicle-insert-string-at-point' is
called successively, this is called to grab more things of text (of
the same kind).  By default, successive words are grabbed.

If either the car or cdr is empty, then the other alone determines the
behavior of `icicle-insert-string-at-point'.  Otherwise, option
`icicle-default-thing-insertion' determines whether the car or cdr is
used by `icicle-insert-string-at-point'.  `C-u' with no number
reverses the meaning of `icicle-default-thing-insertion'."
  :type
  '(cons
    (choice
     (repeat (function :tag "Function to grab some text at point and insert it in minibuffer"))
     (const :tag "No alternative text-grabbing functions" nil))
    (choice
     (const :tag "No function to successively grab more text" nil)
     (function :tag "Function to advance point one text thing")))
  :group 'Icicles-Miscellaneous)

;; Must be before `icicle-top-level-key-bindings'.
(define-widget 'icicle-key-definition 'lazy
  "Key definition type for Icicle mode keys.
A list of three components: KEY, COMMAND, CONDITION, that represents
an `icicle-mode-map' binding of COMMAND according to KEY, if CONDITION
evaluates to non-nil.

KEY is either a key sequence (string or vector) or a command.
COMMAND is a command.
CONDITION is a sexp.

If KEY is a command, then the binding represented is its remapping to
COMMAND."
  :indent 1 :offset 0 :tag ""           ; $$$$$ "Icicle Mode Key Definition"
  :type
  '(list
    (choice
     (key-sequence :tag "Key" :value [ignore])
     ;; Use `symbolp' instead of `commandp', in case the library defining the
     ;; command is not loaded.
     (restricted-sexp :tag "Command to remap" :match-alternatives (symbolp) :value ignore))
     ;; Use `symbolp' instead of `commandp'...
    (restricted-sexp :tag "Command" :match-alternatives (symbolp) :value ignore)
    (sexp :tag "Condition")))

;; Must be before `icicle-top-level-key-bindings'.
(defun icicle-remap (old new map &optional oldmap)
  "Bind command NEW in MAP to all keys currently bound to OLD.
If command remapping is available, use that.  Otherwise, bind NEW to
whatever OLD is bound to in MAP, or in OLDMAP, if provided."
  (if (fboundp 'command-remapping)
      (define-key map (vector 'remap old) new) ; Ignore OLDMAP for Emacs 22.
    (substitute-key-definition old new map oldmap)))

;; Must be before `icicle-top-level-key-bindings'.
(defun icicle-bind-top-level-commands (&optional defs)
  "Bind top-level commands for Icicle mode."
  (unless defs (setq defs  icicle-top-level-key-bindings))
  (let (key command condition)
    (dolist (key-def defs)
      (setq key        (car key-def)
            command    (cadr key-def)
            condition  (car (cddr key-def)))
      (when (eval condition)
        (if (symbolp key)
            (icicle-remap key command icicle-mode-map (current-global-map))
          (define-key icicle-mode-map key command))))))            

;; Must be before `icicle-top-level-key-bindings'.
;;;###autoload
(defcustom icicle-yank-function 'yank
  "*Yank function.  A function that takes a prefix argument.  This
should be a command that is bound to whatever key you use to yank
text, whether in Icicle mode or not.  In Icicle mode, command
`icicle-yank-maybe-completing' calls this function, except when
`icicle-yank-maybe-completing' is called from the minibuffer or called
with a negative prefix argument.  `icicle-yank-maybe-completing'
passes the raw prefix argument to `icicle-yank-function'.

By default (see option `icicle-top-level-key-bindings'), the command
that is the value of this option is remapped to
`icicle-yank-maybe-completing' the first time you enter Icicle mode.
If you customize `icicle-yank-function', then, to take advantage of
this default remapping behavior, you will need to save your
customization and then restart Emacs.

Alternatively, you can customize both `icicle-yank-function' and the
corresponding entry in `icicle-top-level-key-bindings', and then
toggle Icicle mode off and then back on."
  :type 'function :group 'Icicles-Miscellaneous)

;;;###autoload
(defcustom icicle-top-level-key-bindings
  `((,(kbd "<pause>") icicle-switch-to/from-minibuffer    t) ; `pause'
    (,(kbd "C-c `")   icicle-search-generic               t) ; `C-c `'
    (,(kbd "C-c $")   icicle-search-word                  t) ; `C-c $'
    (,(kbd "C-c ^")   icicle-search-keywords              t) ; `C-c ^'
    (,(kbd "C-c '")   icicle-occur                        t) ; `C-c ''
    (,(kbd "C-c =")   icicle-imenu                        t) ; `C-c ='
    (,(kbd "C-c \"")  icicle-search-text-property         t) ; `C-c "'
    (,(kbd "C-c /")   icicle-complete-thesaurus-entry     t) ; `C-c /'
    (,(kbd "C-x M-e") icicle-execute-named-keyboard-macro t) ; `C-x M-e'
    (,(kbd "C-x SPC") icicle-command-abbrev               t) ; `C-x SPC'
    (,(kbd "C-x 5 o") icicle-select-frame                 t) ; `C-x 5 o'
    (,(kbd "C-h C-o") icicle-describe-option-of-type      t) ; `C-h C-o'
    ,@(and (require 'kmacro nil t)      ; (Emacs 22+)
           `((,(kbd "S-<f4>")    icicle-kmacro            t))) ; `S-f4'
    (abort-recursive-edit           icicle-abort-recursive-edit        t) ; `C-]'
    (bookmark-jump                  icicle-bookmark                    t) ; `C-x r b'
    (bookmark-jump-other-window     icicle-bookmark-other-window       t) ; `C-x 4 j j'
    (bookmark-set                   icicle-bookmark-cmd                t) ; `C-x r m'
    (minibuffer-keyboard-quit      icicle-abort-recursive-edit ; `C-g' (minibuffer - `delsel.el')
     (fboundp 'minibuffer-keyboard-quit))
    (delete-window                  icicle-delete-window               t) ; `C-x 0'
    (delete-windows-for             icicle-delete-window           t) ; `C-x 0' (`frame-cmds.el')
    (dired                          icicle-dired                       t) ; `C-x d'
    (dired-other-window             icicle-dired-other-window          t) ; `C-x 4 d'
    (exchange-point-and-mark        icicle-exchange-point-and-mark     t) ; `C-x C-x'
    (execute-extended-command       icicle-execute-extended-command    t) ; `M-x'
    (find-file                      icicle-file                        t) ; `C-x C-f'
    (find-file-other-window         icicle-file-other-window           t) ; `C-x 4 f'
    (find-file-read-only            icicle-find-file-read-only         t) ; `C-x C-r'
    (find-file-read-only-other-window
     icicle-find-file-read-only-other-window                           t) ; `C-x 4 r'
    ;; There are no key bindings in vanilla Emacs for `insert-buffer'.
    ;; If you use `setup-keys.el', then these are its bindings: `C-S-insert', `M-S-f1'.
    (insert-buffer                  icicle-insert-buffer               t)
    (kill-buffer                    icicle-kill-buffer                 t) ; `C-x k'
    (kill-buffer-and-its-windows    icicle-kill-buffer              t) ; `C-x k' (`misc-cmds.el')
    (other-window                 icicle-other-window-or-frame         t) ; `C-x o'
    (other-window-or-frame        icicle-other-window-or-frame     t) ; `C-x o' (`frame-cmds.el')
    (pop-global-mark
     icicle-goto-global-marker-or-pop-global-mark                     t) ; `C-x C-@', `C-x C-SPC'
    (set-mark-command
     icicle-goto-marker-or-set-mark-command                            t) ; `C-@', `C-SPC'
    (switch-to-buffer               icicle-buffer                      t) ; `C-x b'
    (switch-to-buffer-other-window  icicle-buffer-other-window         t) ; `C-x 4 b'
    (where-is                       icicle-where-is                    t) ; `C-h w'
    (,icicle-yank-function          icicle-yank-maybe-completing       t) ; `C-y'

    ;; These are available only if you use library `bookmark+.el'.
    ;;
    ;;   (Other-window means nothing for a bookmark list or a desktop.)
    (bmkp-bookmark-list-jump
     icicle-bookmark-bookmark-list (featurep 'bookmark+))                 ; `C-x j B'
    (bmkp-desktop-jump
     icicle-bookmark-desktop (featurep 'bookmark+))                       ; `C-x j K'
    (bmkp-dired-jump
     icicle-bookmark-dired (featurep 'bookmark+))                         ; `C-x j d'
    (bmkp-dired-jump-other-window
     icicle-bookmark-dired-other-window (featurep 'bookmark+))            ; `C-x 4 j d'
    (bmkp-file-jump
     icicle-bookmark-file (featurep 'bookmark+))                          ; `C-x j f'
    (bmkp-file-jump-other-window
     icicle-bookmark-file-other-window (featurep 'bookmark+))             ; `C-x 4 j f'
    (bmkp-gnus-jump
     icicle-bookmark-gnus (featurep 'bookmark+))                          ; `C-x j g'
    (bmkp-gnus-jump-other-window
     icicle-bookmark-gnus-other-window (featurep 'bookmark+))             ; `C-x 4 j g'
    (bmkp-info-jump
     icicle-bookmark-info (featurep 'bookmark+))                          ; `C-x j i'
    (bmkp-info-jump-other-window
     icicle-bookmark-info-other-window (featurep 'bookmark+))             ; `C-x 4 j i'
    (bmkp-local-file-jump
     icicle-bookmark-local-file (featurep 'bookmark+))                    ; `C-x j l'
    (bmkp-local-file-jump-other-window
     icicle-bookmark-local-file-other-window (featurep 'bookmark+))       ; `C-x 4 j l'
    (bmkp-man-jump
     icicle-bookmark-man  (featurep 'bookmark+))                          ; `C-x j m'
    (bmkp-man-jump-other-window
     icicle-bookmark-man-other-window  (featurep 'bookmark+))             ; `C-x 4 j m'
    (bmkp-non-file-jump
     icicle-bookmark-non-file (featurep 'bookmark+))                      ; `C-x j b'
    (bmkp-non-file-jump-other-window
     icicle-bookmark-non-file-other-window (featurep 'bookmark+))         ; `C-x 4 j b'
    (bmkp-region-jump
     icicle-bookmark-region (featurep 'bookmark+))                        ; `C-x j r'
    (bmkp-region-jump-other-window
     icicle-bookmark-region-other-window (featurep 'bookmark+))           ; `C-x 4 j r'
    (bmkp-remote-file-jump
     icicle-bookmark-remote-file (featurep 'bookmark+))                   ; `C-x j n'
    (bmkp-remote-file-jump-other-window
     icicle-bookmark-remote-file-other-window (featurep 'bookmark+))      ; `C-x 4 j n'
    (bmkp-specific-buffers-jump
     icicle-bookmark-specific-buffers (featurep 'bookmark+))              ; `C-x j = b'
    (bmkp-specific-buffers-jump-other-window
     icicle-bookmark-specific-buffers-other-window (featurep 'bookmark+)) ; `C-x 4 j = b'
    (bmkp-specific-files-jump
     icicle-bookmark-specific-files (featurep 'bookmark+))                ; `C-x j = f'
    (bmkp-specific-files-jump-other-window
     icicle-bookmark-specific-files-other-window (featurep 'bookmark+))   ; `C-x 4 j = f'
    (bmkp-this-buffer-jump
     icicle-bookmark-this-buffer (featurep 'bookmark+))                   ; `C-x j .'
    (bmkp-this-buffer-jump-other-window
     icicle-bookmark-this-buffer-other-window (featurep 'bookmark+))      ; `C-x 4 j .'
    (bmkp-all-tags-jump
     icicle-bookmark-all-tags (featurep 'bookmark+))                      ; `C-x j t *'
    (bmkp-all-tags-jump-other-window
     icicle-bookmark-all-tags-other-window (featurep 'bookmark+))         ; `C-x 4 j t *'
    (bmkp-all-tags-jump
     icicle-bookmark-all-tags-regexp (featurep 'bookmark+))               ; `C-x j t % *'
    (bmkp-all-tags-regexp-jump-other-window
     icicle-bookmark-all-tags-regexp-other-window (featurep 'bookmark+))  ; `C-x 4 j t % *'
    (bmkp-some-tags-jump
     icicle-bookmark-some-tags (featurep 'bookmark+))                     ; `C-x j t +'
    (bmkp-some-tags-jump-other-window
     icicle-bookmark-some-tags-other-window (featurep 'bookmark+))        ; `C-x 4 j t +'
    (bmkp-some-tags-jump
     icicle-bookmark-some-tags-regexp (featurep 'bookmark+))              ; `C-x j t % +'
    (bmkp-some-tags-regexp-jump-other-window
     icicle-bookmark-some-tags-regexp-other-window (featurep 'bookmark+)) ; `C-x 4 j t % +'
    (bmkp-w3m-jump
     icicle-bookmark-w3m-other-window (featurep 'bookmark+))              ; `C-x j w'
    (bmkp-w3m-jump-other-window
     icicle-bookmark-w3m-other-window (featurep 'bookmark+))              ; `C-x 4 j w'

    ;; Don't let Emacs 20 or 21 use `substitute-key-definition' on `M-.' or `M-*', since we need
    ;; these keys for the minibuffer.  Leave them unbound in `icicle-mode-map' until Emacs 22+.
    (find-tag            icicle-find-tag              (fboundp 'command-remapping)) ; `M-.'
    (find-tag-other-window        icicle-find-first-tag-other-window t) ; `C-x 4 .'
    (pop-tag-mark        icicle-pop-tag-mark          (fboundp 'command-remapping)) ; `M-*'
    (eval-expression     icicle-pp-eval-expression    (fboundp 'command-remapping)) ; `M-:'
    (pp-eval-expression icicle-pp-eval-expression (fboundp 'command-remapping)) ;`M-:' (`pp+.el')
    ;; For La Carte (`lacarte.el'), not Icicles, but it's convenient to do this here.
    (,(kbd "ESC M-x")      lacarte-execute-command ; `ESC M-x'
     (fboundp 'lacarte-execute-command))
    (,(kbd "M-`")          lacarte-execute-menu-command ; `M-`' - replaces `tmm-menubar'.
     (fboundp 'lacarte-execute-menu-command))
    (,(kbd "<f10>")        lacarte-execute-menu-command ; `f10' - replaces `menu-bar-open'.
     (fboundp 'lacarte-execute-menu-command)))
  "*List of top-level commands to bind in Icicle mode.
Each list element is of custom type `icicle-key-definition' and has
the form (KEY COMMAND CONDITION).

KEY is either a key sequence (string or vector) to bind COMMAND to or
a command to remap to COMMAND.
COMMAND is bound according to the value of KEY, unless the result of
evaluating CONDITION is nil.

In Customize, to specify a key sequence, choose `Key' in the `Value
Menu', then enter a key description such as that returned by `C-h k'.
For convenience, you can use insert each key in the key description by
hitting `C-q' then the key.  For example, to enter the key description
`C-c M-k' you can use `C-q C-c C-q M-k'.

If you customize this option, then you must exit and re-enter Icicle
mode to ensure that the change takes effect.  This is really necessary
only if your changes would undefine a key.

For this option to have an effect upon startup, it must be set before
you enter Icicle mode.  This means that you must ensure that the code
that sets it is invoked before you enter Icicle mode.  If you use
Customize to change this option, then ensure that the code inserted by
Customize into your `user-init-file' or your `custom-file' is invoked
before you enter Icicle mode."
  :type (if (> emacs-major-version 20)
            '(repeat icicle-key-definition)
          '(repeat
            (list
             (choice
              (restricted-sexp :tag "Key"
               :match-alternatives ((lambda (x) (or (stringp x) (vectorp x))))
               :value [ignore])
              (restricted-sexp :tag "Command to remap"
               ;; Use `symbolp' instead of `commandp', in case the library defining the
               ;; command is not loaded.
               :match-alternatives (symbolp) :value ignore))
             ;; Use `symbolp' instead of `commandp'...
             (restricted-sexp :tag "Command"
              :match-alternatives (symbolp) :value ignore)
             (sexp :tag "Condition"))))
  :set #'(lambda (sym defs)
           (custom-set-default sym defs)
           (icicle-bind-top-level-commands defs))
  :initialize #'custom-initialize-default
  :group 'Icicles-Key-Bindings)

;;;###autoload
(defcustom icicle-top-level-when-sole-completion-delay 0.7
  "*Number of secs to wait to return to top level if only one completion.
This has no effect if `icicle-top-level-when-sole-completion-flag' is
nil.  Editing the completion (typing or deleting a character) before
the delay expires prevents its automatic acceptance.

Do not set this to 0.0.  Set it to slightly more than zero if you want
instant action."
  :type 'number :group 'Icicles-Matching)

;;;###autoload
(defcustom icicle-top-level-when-sole-completion-flag nil
  "*Non-nil means to return to top level if only one matching completion.
The sole completion is accepted."
  :type 'boolean :group 'Icicles-Matching)

;;;###autoload
(defcustom icicle-touche-pas-aux-menus-flag nil
  "*Non-nil means do not add items to menus except Minibuf and Icicles.
This value is used only when Icicles mode is initially established, so
changing this has no effect after Icicles has been loaded.  However,
you can change it and save the new value so it will be used next time.

For this option to have an effect upon startup, it must be set before
you enter Icicle mode.  This means that you must ensure that the code
that sets it is invoked before you enter Icicle mode.  If you use
Customize to change this option, then ensure that the code inserted by
Customize into your `user-init-file' or your `custom-file' is invoked
before you enter Icicle mode."
  :type 'boolean :group 'Icicles-Key-Bindings)

;;;###autoload
(defcustom icicle-transform-function nil ; Toggle with `C-$,'.
  "*Function used to transform the list of completion candidates.
This is applied to the list of initial candidates.
If this is nil, then no transformation takes place.

You can toggle this option at any time from the minibuffer using
`C-$,'.

NOTE: Although this is a user option, you probably do *NOT* want to
customize it.  Icicles commands already \"do the right thing\" when it
comes to candidate transformation.  The value of this option may be
changed by program locally, for use in particular contexts.  For
example, when you use `C-c C-`' (\ `icicle-search-generic') in a
*shell* buffer, Icicles uses this variable with a value of
`icicle-remove-duplicates', to remove duplicate shell commands from
your input history list.

Emacs-Lisp programmers can use this variable to transform the list of
candidates in any way they like.  A typical use is to remove
duplicates, by binding it to `icicle-remove-duplicates' or
`icicle-remove-dups-if-extras'."
  :type '(choice (const :tag "None" nil) function) :group 'Icicles-Completions-Display)

;;;###autoload
(defcustom icicle-type-actions-alist
  '(("buffer"
     (lambda (b) (with-current-buffer b (ps-print-buffer))) ; E.g. showing you can use lambda.
     1-window-frames-on  another-buffer  browse-url-of-buffer  buffer-disable-undo
     buffer-enable-undo  buffer-file-name  buffer-local-variables  buffer-modified-p
     buffer-name  buffer-size  bury-buffer  choose-grep-buffer  current-line-string
     delete-1-window-frames-on  delete-windows-for  delete-windows-on
     delete/iconify-windows-on  describe-buffer-bindings  diff-buffer-with-file
     display-buffer  display-buffer-other-frame  echo-in-buffer  eval-buffer  fontify-buffer
     generate-new-buffer  get-buffer  get-buffer-process  get-buffer-window
     get-buffer-window-list  grepp-choose-grep-buffer  ibuffer-jump-to-buffer
     icicle-char-properties-in-buffer  icicle-delete-window  icicle-delete-windows-on
     icicle-help-line-buffer  icicle-kill-a-buffer  insert-buffer  insert-buffer-substring
     insert-buffer-substring-as-yank  insert-buffer-substring-no-properties  kill-buffer
     kill-buffer-and-its-windows  kill-buffer-if-not-modified  last-buffer
     multi-window-frames-on  other-buffer  pop-to-buffer  pr-interface  remove-windows-on
     replace-buffer-in-windows  smiley-buffer  switch-to-buffer  switch-to-buffer-other-frame
     switch-to-buffer-other-window  view-buffer  view-buffer-other-frame
     view-buffer-other-window)
    ("color"
     color-defined-p  color-gray-p  color-supported-p  color-values  colors
     doremi-set-background-color  doremi-set-foreground-color  eyedrop-color-message
     facemenu-set-background  facemenu-set-foreground  hexrgb-blue  hexrgb-color-name-to-hex
     hexrgb-complement  hexrgb-green  hexrgb-hex-to-color-values  hexrgb-hue  hexrgb-red
     hexrgb-saturation  hexrgb-value  icicle-color-help icicle-color-name-w-bg
     palette-brightness-scale  palette-color-message  palette-complement-or-alternative
     palette-hex-info  palette-hsv-info  palette-rgb-info  palette-swatch  ps-e-color-values
     ps-e-x-color-values  set-background-color  set-border-color  set-cursor-color
     set-foreground-color  set-mouse-color  tty-color-canonicalize  tty-color-desc
     tty-color-standard-values  tty-color-translate  tty-color-values  x-color-defined-p
     x-color-values)
    ("command"
     command-remapping  define-prefix-command  describe-command  disable-command
     doremi-push-frame-config-for-command  enable-command  Info-find-emacs-command-nodes
     Info-goto-emacs-command-node)
    ("face"
     color-theme-spec  custom-facep  customize-face  customize-face-other-window
     custom-theme-recalc-face  describe-face  face-all-attributes  face-attr-construct
     face-background  face-background-20+  face-background-pixmap  face-bold-p
     face-default-spec  face-differs-from-default-p  face-doc-string  face-documentation
     face-font  face-foreground  face-foreground-20+  face-id  face-inverse-video-p
     face-italic-p  face-nontrivial-p  face-spec-reset-face  face-stipple  face-underline-p
     face-user-default-spec  facemenu-add-face facemenu-add-new-face  facemenu-set-face
     find-face-definition  hlt-choose-default-face  hlt-show-default-face
     hlt-unhighlight-region-for-face  icicle-customize-face
     icicle-customize-face-other-window  invert-face  make-face  make-face-bold
     make-face-bold-italic  make-face-italic  make-face-unbold  make-face-unitalic
     moccur-face-check  modify-face  ps-face-attributes  read-all-face-attributes
     read-face-font)
    ("file"
     abbreviate-file-name  ange-ftp-chase-symlinks  ange-ftp-file-modtime  apropos-library  
     auto-coding-alist-lookup  bookmark-file-or-variation-thereof  bookmark-load
     browse-url-of-file  byte-compile-file  check-declare-file  comint-append-output-to-file
     comint-quote-filename  comint-substitute-in-file-name  comint-unquote-filename
     comint-write-output  compilation-get-file-structure  cookie-insert  create-file-buffer
     delete-file  describe-file  dired-delete-file  diredp-mouse-diff  dired-show-file-type
     dir-locals-find-file  dir-locals-read-from-file  do-after-load-evaluation  ebnf-eps-file
     ebnf-print-file  ebnf-spool-file  ebnf-syntax-file  ediff-backup  epa-decrypt-file
     epa-import-keys  epa-verify-file  eval-next-after-load  ffap-file-remote-p
     ffap-locate-file  file-attributes  file-cache-add-file  file-chase-links
     file-dependents  file-directory-p  file-executable-p  file-exists-p
     file-loadhist-lookup  file-local-copy  file-modes  file-name-nondirectory
     file-newest-backup  file-nlinks  file-ownership-preserved-p  file-provides
     file-readable-p  file-regular-p  file-relative-name  file-remote-p  file-requires
     file-symlink-p  file-system-info  file-truename  file-writable-p  find-alternate-file
     find-alternate-file-other-window  find-buffer-visiting  finder-commentary  find-file
     find-file-at-point  find-file-binary  find-file-literally  find-file-noselect
     find-file-other-frame  find-file-other-window find-file-read-only
     find-file-read-only-other-frame  find-file-read-only-other-window  find-file-text
     get-file-buffer  gnus-audio-play  gnus-convert-png-to-face  hexl-find-file
     highlight-compare-with-file   icicle-add-file-to-fileset
     icicle-delete-file-or-directory  icicle-describe-file  icicle-file-remote-p
     icicle-help-line-file  icicle-search-file  icicle-shell-command-on-file
     image-type-from-file-header  image-type-from-file-name  Info-find-file  Info-index-nodes
     info-lookup-file  Info-toc-nodes  info-xref-check  insert-file  insert-file-literally
     insert-image-file  list-tags  lm-commentary  lm-creation-date  lm-keywords  lm-keywords-list
     lm-last-modified-date  lm-summary  lm-synopsis  lm-verify  lm-version  load  load-file
     load-history-regexp  make-backup-file-name  move-file-to-trash  open-dribble-file
     open-termscript  play-sound-file  pr-ps-file-preview  pr-ps-file-print
     pr-ps-file-ps-print  pr-ps-file-using-ghostscript  recentf-add-file  recentf-push
     recentf-remove-if-non-kept  recover-file  rmail-input  rmail-output  set-file-times
     set-visited-file-name  substitute-in-file-name  system-move-file-to-trash
     untranslated-canonical-name  untranslated-file-p  url-basepath  vc-backend  
     vc-delete-automatic-version-backups  vc-file-clearprops  vc-insert-file
     vc-make-version-backup  vc-name  vc-state  vc-working-revision  view-file
     view-file-other-frame  view-file-other-window  visit-tags-table  w32-browser
     w32-long-file-name  w32-short-file-name  w32explore  woman-find-file  write-file
     xml-parse-file)
    ("frame"
     current-window-configuration  delete-frame  delete-other-frames  thumfr-dethumbify-frame
     doremi-undo-last-frame-color-change  thumfr-fisheye  fit-frame  fit-frame-maximize-frame
     fit-frame-minimize-frame  fit-frame-restore-frame  frame-char-height  frame-char-width
     frame-current-scroll-bars  frame-extra-pixels-height  frame-extra-pixels-width
     frame-face-alist  frame-first-window  frame-focus  frame-height  frame-iconified-p
     frame-parameters  frame-pixel-height  frame-pixel-width frame-root-window
     frame-selected-window  frame-set-background-mode  frame-terminal
     frame-update-face-colors  frame-visible-p  frame-width  get-a-frame  get-frame-name
     hide-frame  icicle-select-frame-by-name  iconify-frame  lower-frame
     make-frame-invisible  make-frame-visible  maximize-frame  maximize-frame-horizontally
     maximize-frame-vertically  menu-bar-open  minimize-frame  next-frame
     thumfr-only-raise-frame  previous-frame  raise-frame  really-iconify-frame
     redirect-frame-focus  redraw-frame  restore-frame  restore-frame-horizontally
     restore-frame-vertically  select-frame   select-frame-set-input-focus  set-frame-name
     show-frame  thumfr-thumbify-frame  thumfr-thumbify-other-frames  thumfr-thumbnail-frame-p
     thumfr-toggle-thumbnail-frame  toggle-max-frame  toggle-max-frame-horizontally
     toggle-max-frame-vertically  toggle-zoom-frame  tty-color-alist  tty-color-clear
     w32-focus-frame  window-list  window-system  window-tree  x-focus-frame  zoom-frm-in
     zoom-frm-out  zoom-frm-unzoom)
    ("function"
     cancel-function-timers  describe-function  elp-instrument-function  find-function
     find-function-other-frame  find-function-other-window  symbol-function  trace-function
     trace-function-background)
    ("option" custom-note-var-changed  customize-option  customize-option-other-window
     describe-option  icicle-binary-option-p  tell-customize-var-has-changed)
    ("process"
     accept-process-output  anything-kill-async-process  clone-process  continue-process
     delete-process  get-process  interrupt-process  kill-process  process-buffer
     process-coding-system  process-command  process-contact  process-exit-status
     process-filter  process-filter-multibyte-p  process-id
     process-inherit-coding-system-flag  process-kill-without-query  process-mark
     process-name  process-plist  process-query-on-exit-flag  process-running-child-p
     process-send-eof  process-sentinel  process-status  process-tty-name  process-type
     quit-process  set-process-coding-system  stop-process  tooltip-process-prompt-regexp
     tq-create)
    ("symbol"
     apropos-describe-plist  apropos-macrop  apropos-score-symbol  byte-compile-const-symbol-p
     custom-guess-type  custom-unlispify-tag-name  custom-variable-type  default-boundp
     default-value  describe-minor-mode-from-symbol  fmakunbound
     icicle-help-on-candidate-symbol  info-lookup-symbol  makunbound  symbol-file
     symbol-function  symbol-plist  symbol-value)
    ("variable"
     custom-type  custom-variable-documentation  custom-variable-p  custom-variable-type
     describe-variable  find-variable  find-variable-noselect  find-variable-other-frame
     find-variable-other-window  help-custom-type  icicle-custom-type  kill-local-variable
     local-variable-if-set-p  local-variable-p  make-local-variable  make-variable-buffer-local
     make-variable-frame-local  symbol-value  user-variable-p  variable-binding-locus)
    ("window"
     balance-windows  browse-kill-ring-fit-window  compilation-set-window-height
     delete-other-windows  delete-other-windows-vertically  delete-window
     delete/iconify-window  fit-frame-max-window-size  fit-window-to-buffer
     mouse-drag-vertical-line-rightward-window  mouse-drag-window-above  next-window
     previous-window  remove-window  select-window  shrink-window-if-larger-than-buffer
     split-window  truncated-partial-width-window-p  window--display-buffer-1
     window--even-window-heights  window--try-to-split-window  window-body-height
     window-buffer  window-buffer-height  window-current-scroll-bars  window-dedicated-p
     window-display-table  window-edges  window-end  window-fixed-size-p  window-frame
     window-fringes  window-full-width-p  window-height  window-hscroll  window-inside-edges
     window-inside-pixel-edges  window-margins  window-minibuffer-p  window-parameters
     window-pixel-edges  window-point  window-safely-shrinkable-p  window-scroll-bars
     window-start  window-text-height  window-vscroll  window-width))
  "*Alist of Emacs object types and associated actions.
Each element has the form (TYPE FUNCTION...), where TYPE names an
object type, and each FUNCTION accepts an object of type TYPE as its
only required argument

A FUNCTION here can be a symbol or a lambda form.  You can use a
symbol that is not yet `fboundp', that is, one that does not yet have
a function definition.  Any symbols that do not have function
definitions when this option is used are simply filtered out.

However, just because a function is defined at runtime does not mean
that it will work.  For example, function `buffer-size' is included in
the default value for type `buffer', but in Emacs 20 `buffer-size'
accepts no arguments, so applying it to a buffer name raises an error.

\[Note: If you have suggestions or corrections for the default value,
send them in, using `\\[icicle-send-bug-report]'.  The initial list
was drawn up quickly by looking at functions with the type in their
name and that accept a value of that type as only required argument.
There is no doubt that the default value could be improved.]"
  :type '(alist
          :key-type   (string :tag "Object type")
          ;; We cannot use type `function' because some symbols might not yet be `fboundp'.
          :value-type (repeat (restricted-sexp :tag "Action (function)"
                               :match-alternatives (functionp symbolp))))
  :group 'Icicles-Miscellaneous)

;;;###autoload
(defcustom icicle-unpropertize-completion-result-flag nil
  "*Non-nil means strip text properties from the completion result.
Set this option to non-nil only if you need to ensure, for some other
library, that the string returned by `completing-read' and (starting
with Emacs 23) `read-file-name' has no text properties.

Typically, you will not use a non-nil value.  Internal text properties
added by Icicles are always removed anyway.  A non-nil value lets you
also remove properties such as `face'."
  :type 'boolean :group 'Icicles-Miscellaneous)

;;;###autoload
(defcustom icicle-update-input-hook nil
  "*Functions run when minibuffer input is updated (typing or deleting)."
  :type 'hook :group 'Icicles-Miscellaneous)

;;;###autoload
(defcustom icicle-use-~-for-home-dir-flag t ; Toggle with `M-~'.
  "*Non-nil means abbreviate your home directory using `~'.
You can toggle this option from the minibuffer at any time using
`M-~'."
  :type 'boolean :group 'Icicles-Key-Bindings)

;;;###autoload
(defcustom icicle-use-C-for-actions-flag t ; Toggle with `M-g'.
  "*Non-nil means use modifier `C-' (Control) for multi-command actions.
If nil, then you need no `C-' modifier for actions, and, instead, you
need a `C-' modifier for ordinary candidate cycling.

It is not strictly correct to speak in terms of the `C-' modifier -
that is only the default behavior.  The actual keys concerned are
those defined by these options:

 `icicle-apropos-cycle-next-action-keys'
 `icicle-apropos-cycle-previous-action-keys'
 `icicle-prefix-cycle-next-action-keys'
 `icicle-prefix-cycle-previous-action-keys'
 `icicle-modal-cycle-down-action-keys'
 `icicle-modal-cycle-up-action-keys'

You can toggle this option from the minibuffer at any time using
`M-g'."
  :type 'boolean :group 'Icicles-Key-Bindings)

;;;###autoload
(defcustom icicle-use-anything-candidates-flag t
  "*Non-nil means Icicles can include Anything candidates for completion.
When non-nil, Anything actions are used for candidate alternate
actions in some Icicles commands, and Anything types and actions are
used by command `icicle-object-action' (aka `a' and `what-which-how').

This option has no effect if library `anything.el' cannot be loaded."
  :type 'boolean :group 'Icicles-Matching)

;;;###autoload
(defcustom icicle-use-candidates-only-once-flag nil
  "*Non-nil means remove each candidate from the set after using it.
When you use a multi-command and act on a candidate (for example, with
`C-RET'), the candidate is removed from those available if this is
non-nil.  If this is nil, then the candidate is not removed, so you
can act on it again.

You can customize this option if you prefer the non-nil behavior all
of the time.  However, most users will not want to do that.

If you write Emacs-Lisp code, you can bind this to non-nil during
completion in contexts where it makes little sense for users to act on
the same candidate more than once.  That way, users cannot choose it
again, and they are not distracted seeing it as a candidate.

See also non-option variable `icicle-use-candidates-only-once-alt-p'."
  :type 'boolean :group 'Icicles-Matching)

;;;###autoload
(defcustom icicle-word-completion-keys '([(meta ?\ )])
  "*Key sequences to use for minibuffer prefix word completion.
A list of values that each has the same form as a key-sequence
argument to `define-key'.  It is a list mainly in order to accommodate
different keyboards.

Because file names, in particular, can contain spaces, some people
prefer such a key sequence to be non-printable, such as `M-SPC'.  This
is the default value in Icicles.

But because the spacebar is such a convenient key to hit, other people
prefer to use `SPC' for word completion, and to insert a space some
other way.  The usual way to do that is via `C-q SPC', but command
`icicle-insert-a-space' is provided for convenience.  You can bind
this to `M-SPC', for instance, in `minibuffer-local-completion-map',
`minibuffer-local-completion-map', and
`minibuffer-local-must-match-map'."
  :type '(repeat sexp) :group 'Icicles-Key-Bindings)

;;;###autoload
(defcustom icicle-WYSIWYG-Completions-flag "MMMM"
  "*Non-nil means show candidates in *Completions* using WYSIWYG.
This has an effect only for completion of faces and colors.

The particular non-nil value determines the appearance:
* If t, the candidate is shown with its text properties.
* If a string, the string is propertized and then appended to the
  candidate,  to serve as a color swatch.

Some commands might override a string value with different text.  This
is the case for `icicle-read-color', for instance: the color swatch
text is always the color's RGB code.

Note that, starting with Emacs 22, if this option is non-nil, then
command `describe-face' does not use `completing-read-multiple', since
that (non-Icicles) function does not support WYSIWYG candidates."
  :type '(choice
          (string :tag "Show candidate plus a WYSIWYG swatch with text..."  :value "MMMM")
          (const  :tag "Show candidate itself using WYSIWYG"                t)
          (const  :tag "Show candidate as is, with no text properties"      nil))
  :group 'Icicles-Completions-Display)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'icicles-opt)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; icicles-opt.el ends here
