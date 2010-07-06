;;; yasnippet-bundle.el --- Yet another snippet extension (Auto compiled bundle)
;;; Yasnippet.el --- Yet another snippet extension for Emacs.

;; Copyright 2008 pluskid
;;           2009 pluskid, joaotavora

;; Authors: pluskid <pluskid@gmail.com>, joaotavora <joaotavora@gmail.com>
;; Version: 0.6.1
;; Package-version: 0.6.1c
;; X-URL: http://code.google.com/p/yasnippet/
;; Keywords: convenience, emulation
;; URL: http://code.google.com/p/yasnippet/
;; EmacsWiki: YaSnippetMode

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; Basic steps to setup:
;;
;;   1. In your .emacs file:
;;        (add-to-list 'load-path "/dir/to/yasnippet.el")
;;        (require 'yasnippet)
;;   2. Place the `snippets' directory somewhere.  E.g: ~/.emacs.d/snippets
;;   3. In your .emacs file
;;        (setq yas/root-directory "~/.emacs/snippets")
;;        (yas/load-directory yas/root-directory)
;;   4. To enable the YASnippet menu and tab-trigger expansion
;;        M-x yas/minor-mode
;;   5. To globally enable the minor mode in *all* buffers
;;        M-x yas/global-mode
;;
;;   Steps 4. and 5. are optional, you don't have to use the minor
;;   mode to use YASnippet.
;;
;;   Interesting variables are:
;;
;;       `yas/root-directory'
;;
;;           The directory where user-created snippets are to be
;;           stored. Can also be a list of directories that
;;           `yas/reload-all' will use for bulk-reloading snippets. In
;;           that case the first directory the default for storing new
;;           snippets.
;;
;;       `yas/mode-symbol'
;;
;;           A local variable that you can set in a hook to override
;;           snippet-lookup based on major mode. It is a a symbol (or
;;           list of symbols) that correspond to subdirectories of
;;           `yas/root-directory' and is used for deciding which
;;           snippets to consider for the active buffer.
;;
;;   Major commands are:
;;
;;       M-x yas/expand
;;
;;           Try to expand snippets before point.  In `yas/minor-mode',
;;           this is bound to `yas/trigger-key' which you can customize.
;;
;;       M-x yas/load-directory
;;
;;           Prompts you for a directory hierarchy of snippets to load.
;;
;;       M-x yas/insert-snippet
;;
;;           Prompts you for possible snippet expansion if that is
;;           possible according to buffer-local and snippet-local
;;           expansion conditions.  With prefix argument, ignore these
;;           conditions.
;;
;;       M-x yas/find-snippets
;;
;;           Lets you find the snippet files in the correct
;;           subdirectory of `yas/root-directory', according to the
;;           active major mode (if it exists) like
;;           `find-file-other-window'.
;;
;;       M-x yas/visit-snippet-file
;;
;;           Prompts you for possible snippet expansions like
;;           `yas/insert-snippet', but instead of expanding it, takes
;;           you directly to the snippet definition's file, if it
;;           exists.
;;
;;       M-x yas/new-snippet
;;
;;           Lets you create a new snippet file in the correct
;;           subdirectory of `yas/root-directory', according to the
;;           active major mode.
;;
;;       M-x yas/load-snippet-buffer
;;
;;           When editing a snippet, this loads the snippet.  This is
;;           bound to "C-c C-c" while in the `snippet-mode' editing
;;           mode.
;;
;;       M-x yas/tryout-snippet
;;
;;           When editing a snippet, this opens a new empty buffer,
;;           sets it to the appropriate major mode and inserts the
;;           snippet there, so you can see what it looks like.  This is
;;           bound to "C-c C-t" while in `snippet-mode'.
;;
;;   The `dropdown-list.el' extension is bundled with YASnippet, you
;;   can optionally use it the preferred "prompting method", puting in
;;   your .emacs file, for example:
;;
;;       (require 'dropdown-list)
;;       (setq yas/prompt-functions '(yas/dropdown-prompt
;;                                    yas/ido-prompt
;;                                    yas/completing-prompt))
;;
;;   Also check out the customization group
;;
;;        M-x customize-group RET yasnippet RET
;;
;;   If you use the customization group to set variables
;;   `yas/root-directory' or `yas/global-mode', make sure the path to
;;   "yasnippet.el" is present in the `load-path' *before* the
;;   `custom-set-variables' is executed in your .emacs file.
;;
;;   For more information and detailed usage, refer to the project page:
;;      http://code.google.com/p/yasnippet/

;;; Code:

(require 'cl)
(require 'assoc)
(require 'easymenu)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; User customizable variables


(defgroup yasnippet nil
  "Yet Another Snippet extension"
  :group 'editing)

;;;###autoload
(defcustom yas/root-directory nil
  "Root directory that stores the snippets for each major mode.

If you set this from your .emacs, can also be a list of strings,
for multiple root directories. If you make this a list, the first
element is always the user-created snippets directory. Other
directories are used for bulk reloading of all snippets using
`yas/reload-all'"
  :type '(choice (string :tag "Single directory (string)")
                 (repeat :args (string) :tag "List of directories (strings)"))
  :group 'yasnippet
  :require 'yasnippet
  :set #'(lambda (symbol new)
           (let ((old (and (boundp symbol)
                           (symbol-value symbol))))
             (set-default symbol new)
             (unless (or (not (fboundp 'yas/reload-all))
                         (equal old new))
               (yas/reload-all)))))

(defcustom yas/prompt-functions '(yas/x-prompt
                                  yas/dropdown-prompt
                                  yas/completing-prompt
                                  yas/ido-prompt
                                  yas/no-prompt)
  "Functions to prompt for keys, templates, etc interactively.

These functions are called with the following arguments:

- PROMPT: A string to prompt the user

- CHOICES: a list of strings or objects.

- optional DISPLAY-FN : A function that, when applied to each of
the objects in CHOICES will return a string.

The return value of any function you put here should be one of
the objects in CHOICES, properly formatted with DISPLAY-FN (if
that is passed).

- To signal that your particular style of prompting is
unavailable at the moment, you can also have the function return
nil.

- To signal that the user quit the prompting process, you can
signal `quit' with

  (signal 'quit \"user quit!\")."
  :type '(repeat function)
  :group 'yasnippet)

(defcustom yas/indent-line 'auto
  "Controls indenting applied to a recent snippet expansion.

The following values are possible:

- `fixed' Indent the snippet to the current column;

- `auto' Indent each line of the snippet with `indent-according-to-mode'

Every other value means don't apply any snippet-side indendation
after expansion (the manual per-line \"$>\" indentation still
applies)."
  :type '(choice (const :tag "Nothing"  nothing)
                 (const :tag "Fixed"    fixed)
                 (const :tag "Auto"     auto))
  :group 'yasnippet)

(defcustom yas/also-auto-indent-first-line nil
  "Non-nil means also auto indent first line according to mode.

Naturally this is only valid when `yas/indent-line' is `auto'"
  :type 'boolean
  :group 'yasnippet)

(defcustom yas/snippet-revival t
  "Non-nil means re-activate snippet fields after undo/redo."
  :type 'boolean
  :group 'yasnippet)

(defcustom yas/trigger-key "TAB"
  "The key bound to `yas/expand' when function `yas/minor-mode' is active.

Value is a string that is converted to the internal Emacs key
representation using `read-kbd-macro'."
  :type 'string
  :group 'yasnippet
  :set #'(lambda (symbol key)
           (let ((old (and (boundp symbol)
                           (symbol-value symbol))))
             (set-default symbol key)
             ;; On very first loading of this defcustom,
             ;; `yas/trigger-key' is *not* loaded.
             (if (fboundp 'yas/trigger-key-reload)
                 (yas/trigger-key-reload old)))))
  
(defcustom yas/next-field-key '("TAB" "<tab>")
  "The key to navigate to next field when a snippet is active.

Value is a string that is converted to the internal Emacs key
representation using `read-kbd-macro'.

Can also be a list of strings."
  :type '(choice (string :tag "String")
                 (repeat :args (string) :tag "List of strings"))
  :group 'yasnippet
  :set #'(lambda (symbol val)
           (set-default symbol val)
           (if (fboundp 'yas/init-yas-in-snippet-keymap)
               (yas/init-yas-in-snippet-keymap))))
           

(defcustom yas/prev-field-key '("<backtab>" "<S-tab>")
  "The key to navigate to previous field when a snippet is active.

Value is a string that is converted to the internal Emacs key
representation using `read-kbd-macro'.

Can also be a list of strings."
  :type '(choice (string :tag "String")
                 (repeat :args (string) :tag "List of strings"))
  :group 'yasnippet
  :set #'(lambda (symbol val)
           (set-default symbol val)
           (if (fboundp 'yas/init-yas-in-snippet-keymap)
               (yas/init-yas-in-snippet-keymap))))

(defcustom yas/skip-and-clear-key "C-d"
  "The key to clear the currently active field.

Value is a string that is converted to the internal Emacs key
representation using `read-kbd-macro'.

Can also be a list of strings."
  :type '(choice (string :tag "String")
                 (repeat :args (string) :tag "List of strings"))
  :group 'yasnippet
  :set #'(lambda (symbol val)
           (set-default symbol val)
           (if (fboundp 'yas/init-yas-in-snippet-keymap)
               (yas/init-yas-in-snippet-keymap))))

(defcustom yas/triggers-in-field nil
  "If non-nil, `yas/next-field-key' can trigger stacked expansions.

Otherwise, `yas/next-field-key' just tries to move on to the next
field"
  :type 'boolean
  :group 'yasnippet)

(defcustom yas/fallback-behavior 'call-other-command
  "How to act when `yas/trigger-key' does *not* expand a snippet.

- `call-other-command' means try to temporarily disable YASnippet
    and call the next command bound to `yas/trigger-key'.

- nil or the symbol `return-nil' mean do nothing. (and
  `yas/expand-returns' nil)

- A lisp form (apply COMMAND . ARGS) means interactively call
  COMMAND, if ARGS is non-nil, call COMMAND non-interactively
  with ARGS as arguments."
  :type '(choice (const :tag "Call previous command"  call-other-command)
                 (const :tag "Do nothing"             return-nil))
  :group 'yasnippet)
(make-variable-buffer-local 'yas/fallback-behavior)

(defcustom yas/choose-keys-first nil
  "If non-nil, prompt for snippet key first, then for template.

Otherwise prompts for all possible snippet names.

This affects `yas/insert-snippet' and `yas/visit-snippet-file'."
  :type 'boolean
  :group 'yasnippet)

(defcustom yas/choose-tables-first nil
  "If non-nil, and multiple eligible snippet tables, prompts user for tables first.

Otherwise, user chooses between the merging together of all
eligible tables.

This affects `yas/insert-snippet', `yas/visit-snippet-file'"
  :type 'boolean
  :group 'yasnippet)

(defcustom yas/use-menu 'real-modes
  "Display a YASnippet menu in the menu bar.

When non-nil, submenus for each snippet table will be listed
under the menu \"Yasnippet\".

- If set to `real-modes' only submenus whose name more or less
corresponds to a major mode are listed.

- If set to `abbreviate', only the current major-mode
menu and the modes set in `yas/mode-symbol' are listed.

Any other non-nil value, every submenu is listed."
  :type '(choice (const :tag "Full"  t)
                 (const :tag "Real modes only" real-modes)
                 (const :tag "Abbreviate" abbreviate))
  :group 'yasnippet)

(defcustom yas/trigger-symbol " =>"
  "The text that will be used in menu to represent the trigger."
  :type 'string
  :group 'yasnippet)

(defcustom yas/wrap-around-region nil
  "If non-nil, snippet expansion wraps around selected region.

The wrapping occurs just before the snippet's exit marker.  This
can be overriden on a per-snippet basis."
  :type 'boolean
  :group 'yasnippet)

(defcustom yas/good-grace t
  "If non-nil, don't raise errors in inline elisp evaluation.

An error string \"[yas] error\" is returned instead."
  :type 'boolean
  :group 'yasnippet)

(defcustom yas/ignore-filenames-as-triggers nil
  "If non-nil, don't derive tab triggers from filenames.

This means a snippet without a \"# key:'\ directive wont have a
tab trigger."
  :type 'boolean
  :group 'yasnippet)

(defcustom yas/visit-from-menu nil
  "If non-nil visit snippets's files from menu, instead of expanding them.

This cafn only work when snippets are loaded from files."
  :type 'boolean
  :group 'yasnippet)

(defface yas/field-highlight-face
  '((((class color) (background light)) (:background "DarkSeaGreen1"))
    (t (:background "DimGrey")))
  "The face used to highlight the currently active field of a snippet"
  :group 'yasnippet)

(defface yas/field-debug-face
  '()
  "The face used for debugging some overlays normally hidden"
  :group 'yasnippet)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; User can also customize the next defvars
(defun yas/define-some-keys (keys keymap definition)
  "Bind KEYS to DEFINITION in KEYMAP, read with `read-kbd-macro'."
  (let ((keys (or (and (listp keys) keys)
                  (list keys))))
    (dolist (key keys)
      (define-key keymap (read-kbd-macro key) definition))))

(defvar yas/keymap
  (let ((map (make-sparse-keymap)))
    (mapc #'(lambda (binding)
              (yas/define-some-keys (car binding) map (cdr binding)))
          `((,yas/next-field-key     . yas/next-field-or-maybe-expand)
            (,yas/prev-field-key     . yas/prev-field)
            ("C-g"                   . yas/abort-snippet)
            (,yas/skip-and-clear-key . yas/skip-and-clear-or-delete-char)))
    map)
  "The keymap active while a snippet expansion is in progress.")

(defvar yas/key-syntaxes (list "w" "w_" "w_." "^ ")
  "A list of syntax of a key. This list is tried in the order
to try to find a key. For example, if the list is '(\"w\" \"w_\").
And in emacs-lisp-mode, where \"-\" has the syntax of \"_\":

foo-bar

will first try \"bar\", if not found, then \"foo-bar\" is tried.")

(defvar yas/after-exit-snippet-hook
  '()
  "Hooks to run after a snippet exited.

The hooks will be run in an environment where some variables bound to
proper values:

`yas/snippet-beg' : The beginning of the region of the snippet.

`yas/snippet-end' : Similar to beg.

Attention: These hooks are not run when exiting nested/stackd snippet expansion!")

(defvar yas/before-expand-snippet-hook
  '()
  "Hooks to run just before expanding a snippet.")

(defvar yas/buffer-local-condition
  '(if (and (not (bobp))
            (or (equal 'font-lock-comment-face
                       (get-char-property (1- (point))
                                          'face))
                (equal 'font-lock-string-face
                       (get-char-property (1- (point))
                                          'face))))
       '(require-snippet-condition . force-in-comment)
     t)
  "Snippet expanding condition.

This variable is a lisp form:

    * If it evaluates to nil, no snippets can be expanded.

    * If it evaluates to the a cons (require-snippet-condition
      . REQUIREMENT)

       * Snippets bearing no \"# condition:\" directive are not
         considered

       * Snippets bearing conditions that evaluate to nil (or
         produce an error) won't be onsidered.

       * If the snippet has a condition that evaluates to non-nil
         RESULT:

          * If REQUIREMENT is t, the snippet is considered

          * If REQUIREMENT is `eq' RESULT, the snippet is
            considered

          * Otherwise, the snippet is not considered.

    * If it evaluates to the symbol 'always, all snippets are
      considered for expansion, regardless of any conditions.

    * If it evaluates to t or some other non-nil value

       * Snippet bearing no conditions, or conditions that
         evaluate to non-nil, are considered for expansion. 

       * Otherwise, the snippet is not considered.

Here's an example preventing snippets from being expanded from
inside comments, in `python-mode' only, with the exception of
snippets returning the symbol 'force-in-comment in their
conditions.

 (add-hook 'python-mode-hook
           '(lambda ()
              (setq yas/buffer-local-condition
                    '(if (python-in-string/comment)
                         '(require-snippet-condition . force-in-comment)
                       t))))

The default value is similar, it filters out potential snippet
expansions inside comments and string literals, unless the
snippet itself contains a condition that returns the symbol
`force-in-comment'.")
(make-variable-buffer-local 'yas/buffer-local-condition)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Internal variables

(defvar yas/version "0.6.1b")

(defvar yas/menu-table (make-hash-table)
  "A hash table of MAJOR-MODE symbols to menu keymaps.")

(defvar yas/active-keybindings nil
  "A list of cons (KEYMAP . KEY) setup from defining snippets.")

(defvar yas/known-modes
  '(ruby-mode rst-mode markdown-mode)
  "A list of mode which is well known but not part of emacs.")

(defvar yas/escaped-characters
  '(?\\ ?` ?' ?$ ?} )
  "List of characters which *might* need to be escaped.")

(defconst yas/field-regexp
  "${\\([0-9]+:\\)?\\([^}]*\\)}"
  "A regexp to *almost* recognize a field.")

(defconst yas/multi-dollar-lisp-expression-regexp
  "$+[ \t\n]*\\(([^)]*)\\)"
  "A regexp to *almost* recognize a \"$(...)\" expression.")

(defconst yas/backquote-lisp-expression-regexp
  "`\\([^`]*\\)`"
  "A regexp to recognize a \"`lisp-expression`\" expression." )

(defconst yas/transform-mirror-regexp
  "${\\(?:\\([0-9]+\\):\\)?$\\([ \t\n]*([^}]*\\)"
  "A regexp to *almost* recognize a mirror with a transform.")

(defconst yas/simple-mirror-regexp
  "$\\([0-9]+\\)"
  "A regexp to recognize a simple mirror.")

(defvar yas/snippet-id-seed 0
  "Contains the next id for a snippet.")

(defun yas/snippet-next-id ()
  (let ((id yas/snippet-id-seed))
    (incf yas/snippet-id-seed)
    id))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Minor mode stuff

;; XXX: `last-buffer-undo-list' is somehow needed in Carbon Emacs for MacOSX
(defvar last-buffer-undo-list nil)

(defvar yas/minor-mode-menu nil
  "Holds the YASnippet menu")

(defun yas/init-minor-keymap ()
  (let ((map (make-sparse-keymap)))
    (easy-menu-define yas/minor-mode-menu
      map
      "Menu used when YAS/minor-mode is active."
      '("YASnippet"
        "----"
        ["Expand trigger" yas/expand
         :help "Possibly expand tab trigger before point"]
        ["Insert at point..." yas/insert-snippet
         :help "Prompt for an expandable snippet and expand it at point"]
        ["New snippet..." yas/new-snippet
         :help "Create a new snippet in an appropriate directory"]
        ["Visit snippet file..." yas/visit-snippet-file
         :help "Prompt for an expandable snippet and find its file"]
        ["Find snippets..." yas/find-snippets
         :help "Invoke `find-file' in the appropriate snippet directory"] 
        "----"
        ("Snippet menu behaviour"
         ["Visit snippets" (setq yas/visit-from-menu t)
          :help "Visit snippets from the menu"
          :active t :style radio   :selected yas/visit-from-menu]
         ["Expand snippets" (setq yas/visit-from-menu nil)
          :help "Expand snippets from the menu"
          :active t :style radio :selected (not yas/visit-from-menu)]
         "----"
         ["Show \"Real\" modes only" (setq yas/use-menu 'real-modes)
          :help "Show snippet submenus for modes that appear to be real major modes"
          :active t :style radio   :selected (eq yas/use-menu 'real-modes)]
         ["Show all modes" (setq yas/use-menu 't)
          :help "Show one snippet submenu for each loaded table"
          :active t :style radio   :selected (eq yas/use-menu 't)]
         ["Abbreviate according to current mode" (setq yas/use-menu 'abbreviate)
          :help "Show only snippet submenus for the current active modes"
          :active t :style radio   :selected (eq yas/use-menu 'abbreviate)])
        ("Indenting"
         ["Auto" (setq yas/indent-line 'auto)
          :help "Indent each line of the snippet with `indent-according-to-mode'"
          :active t :style radio   :selected (eq yas/indent-line 'auto)]
         ["Fixed" (setq yas/indent-line 'fixed)
          :help "Indent the snippet to the current column"
          :active t :style radio   :selected (eq yas/indent-line 'fixed)]
         ["None" (setq yas/indent-line 'none)
          :help "Don't apply any particular snippet indentation after expansion"
          :active t :style radio   :selected (not (member yas/indent-line '(fixed auto)))]
         "----"
         ["Also auto indent first line" (setq yas/also-auto-indent-first-line
                                              (not yas/also-auto-indent-first-line))
          :help "When auto-indenting also, auto indent the first line menu"
          :active (eq yas/indent-line 'auto)
          :style toggle :selected yas/also-auto-indent-first-line]
         )
        ("Prompting method"
         ["System X-widget" (setq yas/prompt-functions
                                  (cons 'yas/x-prompt
                                        (remove 'yas/x-prompt
                                                yas/prompt-functions)))
          :help "Use your windowing system's (gtk, mac, windows, etc...) default menu"
          :active t :style radio   :selected (eq (car yas/prompt-functions)
                                                 'yas/x-prompt)]
         ["Dropdown-list" (setq yas/prompt-functions
                                (cons 'yas/dropdown-prompt
                                      (remove 'yas/dropdown-prompt
                                              yas/prompt-functions)))
          :help "Use a special dropdown list"
          :active t :style radio   :selected (eq (car yas/prompt-functions)
                                                 'yas/dropdown-prompt)]
         ["Ido" (setq yas/prompt-functions
                      (cons 'yas/ido-prompt
                            (remove 'yas/ido-prompt
                                    yas/prompt-functions)))
          :help "Use an ido-style minibuffer prompt"
          :active t :style radio   :selected (eq (car yas/prompt-functions)
                                                 'yas/ido-prompt)]
         ["Completing read" (setq yas/prompt-functions
                                  (cons 'yas/completing-prompt
                                        (remove 'yas/completing-prompt-prompt
                                                yas/prompt-functions)))
          :help "Use a normal minibuffer prompt"
          :active t :style radio   :selected (eq (car yas/prompt-functions)
                                                 'yas/completing-prompt-prompt)]
         )
        ("Misc"
         ["Wrap region in exit marker" 
          (setq yas/wrap-around-region
                (not yas/wrap-around-region))
          :help "If non-nil automatically wrap the selected text in the $0 snippet exit"
          :style toggle :selected yas/wrap-around-region]
         ["Allow stacked expansions " 
          (setq yas/triggers-in-field
                (not yas/triggers-in-field))
          :help "If non-nil allow snippets to be triggered inside other snippet fields"
          :style toggle :selected yas/triggers-in-field]
         ["Revive snippets on undo " 
          (setq yas/snippet-revival
                (not yas/snippet-revival))
          :help "If non-nil allow snippets to become active again after undo"
          :style toggle :selected yas/snippet-revival]
         ["Good grace " 
          (setq yas/good-grace
                (not yas/good-grace))
          :help "If non-nil don't raise errors in bad embedded eslip in snippets"
          :style toggle :selected yas/good-grace]
         ["Ignore filenames as triggers" 
          (setq yas/ignore-filenames-as-triggers
                (not yas/ignore-filenames-as-triggers))
          :help "If non-nil don't derive tab triggers from filenames"
          :style toggle :selected yas/ignore-filenames-as-triggers]
         )
        "----"
        ["Load snippets..."  yas/load-directory
         :help "Load snippets from a specific directory"]
        ["Reload everything" yas/reload-all
         :help "Cleanup stuff, reload snippets, rebuild menus"]
        ["About"            yas/about
         :help "Display some information about YASsnippet"]))
    ;; Now for the stuff that has direct keybindings
    ;;
    (define-key map "\C-c&\C-s" 'yas/insert-snippet)
    (define-key map "\C-c&\C-n" 'yas/new-snippet)
    (define-key map "\C-c&\C-v" 'yas/visit-snippet-file)
    (define-key map "\C-c&\C-f" 'yas/find-snippets)
    map))

(defvar yas/minor-mode-map (yas/init-minor-keymap)
  "The keymap used when `yas/minor-mode' is active.")

(defun yas/trigger-key-reload (&optional unbind-key)
  "Rebind `yas/expand' to the new value of `yas/trigger-key'.

With optional UNBIND-KEY, try to unbind that key from
`yas/minor-mode-map'."
  (when (and unbind-key
             (stringp unbind-key)
             (not (string= unbind-key "")))
    (define-key yas/minor-mode-map (read-kbd-macro unbind-key) nil)) 
  (when  (and yas/trigger-key
              (stringp yas/trigger-key)
              (not (string= yas/trigger-key "")))
    (define-key yas/minor-mode-map (read-kbd-macro yas/trigger-key) 'yas/expand)))

;;;###autoload
(define-minor-mode yas/minor-mode
  "Toggle YASnippet mode.

When YASnippet mode is enabled, the `tas/trigger-key' key expands
snippets of code depending on the mode.

With no argument, this command toggles the mode.
positive prefix argument turns on the mode.
Negative prefix argument turns off the mode.

You can customize the key through `yas/trigger-key'.

Key bindings:
\\{yas/minor-mode-map}"
  nil
  ;; The indicator for the mode line.
  " yas"
  :group 'yasnippet
  (when yas/minor-mode
    (yas/trigger-key-reload)
    ;; load all snippets definitions unless we still don't have a
    ;; root-directory or some snippets have already been loaded.
    (unless (or (null yas/root-directory)
                (> (hash-table-count yas/snippet-tables) 0))
      (yas/reload-all))))

(defvar yas/dont-activate #'(lambda ()
                              (and yas/root-directory
                                   (null (yas/get-snippet-tables))))
  "If non-nil don't let `yas/minor-mode-on' active yas for this buffer.

`yas/minor-mode-on' is usually called by `yas/global-mode' so
this effectively lets you define exceptions to the \"global\"
behaviour.")
(make-variable-buffer-local 'yas/dont-activate)


(defun yas/minor-mode-on ()
  "Turn on YASnippet minor mode.

Do this unless `yas/dont-activate' is t or the function
`yas/get-snippet-tables' (which see), returns an empty list."
  (interactive)
  (unless (or (and (functionp yas/dont-activate)
                   (funcall yas/dont-activate))
              (and (not (functionp yas/dont-activate))
                   yas/dont-activate))
    (yas/minor-mode 1)))

(defun yas/minor-mode-off ()
  "Turn off YASnippet minor mode."
  (interactive)
  (yas/minor-mode -1))

(define-globalized-minor-mode yas/global-mode yas/minor-mode yas/minor-mode-on
  :group 'yasnippet
  :require 'yasnippet)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Major mode stuff
;;
(defvar yas/font-lock-keywords
  (append '(("^#.*$" . font-lock-comment-face))
          lisp-font-lock-keywords
          lisp-font-lock-keywords-1
          lisp-font-lock-keywords-2
          '(("$\\([0-9]+\\)"
             (0 font-lock-keyword-face)
             (1 font-lock-string-face t))
            ("${\\([0-9]+\\):?"
             (0 font-lock-keyword-face)
             (1 font-lock-warning-face t))
            ("${" font-lock-keyword-face)
            ("$[0-9]+?" font-lock-preprocessor-face)
            ("\\(\\$(\\)" 1 font-lock-preprocessor-face)
            ("}"
             (0 font-lock-keyword-face)))))

(defun yas/init-major-keymap ()
  (let ((map (make-sparse-keymap)))
    (easy-menu-define nil
      map
      "Menu used when snippet-mode is active."
      (cons "Snippet"
            (mapcar #'(lambda (ent)
                        (when (third ent)
                          (define-key map (third ent) (second ent)))
                        (vector (first ent) (second ent) t))
                    (list
                     (list "Load this snippet" 'yas/load-snippet-buffer "\C-c\C-c")
                     (list "Try out this snippet" 'yas/tryout-snippet "\C-c\C-t")))))
    map))

(defvar snippet-mode-map
  (yas/init-major-keymap)
  "The keymap used when `snippet-mode' is active")


(define-derived-mode snippet-mode text-mode "Snippet"
  "A mode for editing yasnippets"
  (set-syntax-table (standard-syntax-table))
  (setq font-lock-defaults '(yas/font-lock-keywords))
  (set (make-local-variable 'require-final-newline) nil)
  (use-local-map snippet-mode-map))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Internal structs for template management

(defstruct (yas/template (:constructor yas/make-template
                                       (content name condition expand-env file keybinding)))
  "A template for a snippet."
  content
  name
  condition
  expand-env
  file
  keybinding)

(defvar yas/snippet-tables (make-hash-table)
  "A hash table of MAJOR-MODE symbols to `yas/snippet-table' objects.")

(defstruct (yas/snippet-table (:constructor yas/make-snippet-table (name)))
  "A table to store snippets for a particular mode.

Has the following fields:

`yas/snippet-table-name'

  A symbol normally corresponding to a major mode, but can also be
  a pseudo major-mode to be referenced in `yas/mode-symbol', for
  example.

`yas/snippet-table-hash'

  A hash table the key is a string (the snippet key) and the
  value is yet another hash of (NAME TEMPLATE), where NAME is the
  snippet name and TEMPLATE is a `yas/template' object name.

`yas/snippet-table-parents'

  A list of tables considered parents of this table: i.e. when
  searching for expansions they are searched as well."
  name
  (hash (make-hash-table :test 'equal))
  (parents nil))

(defvar yas/better-guess-for-replacements nil
  "If non-nil `yas/store' better guess snippet replacements.")

(defun yas/store (table name key template)
  "Store a snippet template in the TABLE."

  ;; This is dones by searching twice:
  ;;
  ;; * Try to get the existing namehash from TABLE using key.
  ;;
  ;; * Try to get the existing namehash from by searching the *whole*
  ;; snippet table for NAME. This is becuase they user might have
  ;; changed the key and that can no longer be used to locate the
  ;; previous `yas/template-structure'.
  ;;
  ;; * If that returns nothing, oh well...
  ;;
  (dolist (existing-namehash (remove nil (list (gethash key (yas/snippet-table-hash table))
                                               (when yas/better-guess-for-replacements
                                                 (let (a)
                                                   (maphash #'(lambda (key namehash)
                                                                (when (gethash name namehash)
                                                                  (setq a namehash)))
                                                            (yas/snippet-table-hash table))
                                                   a)))))
    (let ((existing-template (gethash name existing-namehash)))
      (when existing-template
        ;; Remove the existing keybinding
        (when (yas/template-keybinding existing-template)
          (define-key
            (symbol-value (first (yas/template-keybinding existing-template)))
            (second (yas/template-keybinding existing-template))
            nil)
          (setq yas/active-keybindings
                (delete (yas/template-keybinding existing-template)
                        yas/active-keybindings)))
        ;; Remove the (name . template) mapping from existing-namehash.
        (remhash name existing-namehash))))
  ;; Now store the new template independent of the previous steps.
  ;;
  (puthash name
           template
           (or (gethash key
                        (yas/snippet-table-hash table))
               (puthash key
                        (make-hash-table :test 'equal)
                        (yas/snippet-table-hash table)))))

(defun yas/fetch (table key)
  "Fetch a snippet binding to KEY from TABLE."
  (let* ((keyhash (yas/snippet-table-hash table))
         (namehash (and keyhash (gethash key keyhash))))
    (when namehash
      (yas/filter-templates-by-condition
       (let (alist)
         (maphash #'(lambda (k v)
                      (push (cons k v) alist))
                  namehash)
         alist)))))


;; Filtering/condition logic

(defun yas/eval-condition (condition)
  (condition-case err
      (save-excursion
        (save-restriction
          (save-match-data
            (eval condition))))
    (error (progn
             (message (format "[yas] error in condition evaluation: %s"
                              (error-message-string err)))
             nil))))


(defun yas/filter-templates-by-condition (templates)
  "Filter the templates using the applicable condition.

TEMPLATES is a list of cons (NAME . TEMPLATE) where NAME is a
string and TEMPLATE is a `yas/template' structure.

This function implements the rules described in
`yas/buffer-local-condition'.  See that variables documentation."
  (let ((requirement (yas/require-template-specific-condition-p)))
    (if (eq requirement 'always)
        templates
      (remove-if-not #'(lambda (pair)
                         (yas/template-can-expand-p (yas/template-condition (cdr pair)) requirement))
                     templates))))

(defun yas/require-template-specific-condition-p ()
  "Decides if this buffer requests/requires snippet-specific
conditions to filter out potential expansions."
  (if (eq 'always yas/buffer-local-condition)
      'always
    (let ((local-condition (or (and (consp yas/buffer-local-condition)
                                    (yas/eval-condition yas/buffer-local-condition))
                               yas/buffer-local-condition)))
      (when local-condition
        (if (eq local-condition t)
            t
          (and (consp local-condition)
               (eq 'require-snippet-condition (car local-condition))
               (symbolp (cdr local-condition))
               (cdr local-condition)))))))

(defun yas/template-can-expand-p (condition &optional requirement)
  "Evaluates CONDITION and REQUIREMENT and returns a boolean"
  (let* ((requirement (or requirement
                          (yas/require-template-specific-condition-p)))
         (result (or (null condition)
                     (yas/eval-condition
                      (condition-case err
                          (read condition)
                        (error (progn
                                 (message (format "[yas] error reading condition: %s"
                                                  (error-message-string err))))
                               nil))))))
    (cond ((eq requirement t)
           result)
          (t
           (eq requirement result)))))

(defun yas/snippet-table-get-all-parents (table)
  (let ((parents (yas/snippet-table-parents table)))
    (when parents
      (append (copy-list parents)
              (mapcan #'yas/snippet-table-get-all-parents parents)))))

(defun yas/snippet-table-templates (table)
  (when table
    (let ((acc (list)))
      (maphash #'(lambda (key namehash)
                   (maphash #'(lambda (name template)
                                (push (cons name template) acc))
                            namehash))
               (yas/snippet-table-hash table))
      (yas/filter-templates-by-condition acc))))

(defun yas/current-key ()
  "Get the key under current position. A key is used to find
the template of a snippet in the current snippet-table."
  (let ((start (point))
        (end (point))
        (syntaxes yas/key-syntaxes)
        syntax
        done
        templates)
    (while (and (not done) syntaxes)
      (setq syntax (car syntaxes))
      (setq syntaxes (cdr syntaxes))
      (save-excursion
        (skip-syntax-backward syntax)
        (setq start (point)))
      (setq templates
            (mapcan #'(lambda (table)
                        (yas/fetch table (buffer-substring-no-properties start end)))
                    (yas/get-snippet-tables)))
      (if templates
          (setq done t)
        (setq start end)))
    (list templates
          start
          end)))


(defun yas/snippet-table-all-keys (table)
  (when table
    (let ((acc))
      (maphash #'(lambda (key templates)
                   (when (yas/filter-templates-by-condition templates)
                     (push key acc)))
               (yas/snippet-table-hash table))
      acc)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Internal functions

(defun yas/real-mode? (mode)
  "Try to find out if MODE is a real mode. The MODE bound to
a function (like `c-mode') is considered real mode. Other well
known mode like `ruby-mode' which is not part of Emacs might
not bound to a function until it is loaded. So yasnippet keeps
a list of modes like this to help the judgement."
  (or (fboundp mode)
      (find mode yas/known-modes)))

(defun yas/read-and-eval-string (string)
  ;; TODO: This is a possible optimization point, the expression could
  ;; be stored in cons format instead of string,
  "Evaluate STRING and convert the result to string."
  (let ((retval (catch 'yas/exception
                  (condition-case err
                      (save-excursion
                        (save-restriction
                          (save-match-data
                            (widen)
                            (let ((result (eval (read string))))
                              (when result
                                (format "%s" result))))))
                    (error (if yas/good-grace
                               "[yas] elisp error!"
                             (error (format "[yas] elisp error: %s"
                                            (error-message-string err)))))))))
    (when (and (consp retval)
               (eq 'yas/exception (car retval)))
      (error (cdr retval)))
    retval))

(defvar yas/mode-symbol nil
  "If non-nil, lookup snippets using this instead of `major-mode'.")
(make-variable-buffer-local 'yas/mode-symbol)

(defun yas/snippet-table-get-create (mode)
  "Get the snippet table corresponding to MODE.

Optional DIRECTORY gets recorded as the default directory to
search for snippet files if the retrieved/created table didn't
already have such a property."
  (let ((table (gethash mode
                        yas/snippet-tables)))
    (unless table
      (setq table (yas/make-snippet-table (symbol-name mode)))
      (puthash mode table yas/snippet-tables))
    table))

(defun yas/get-snippet-tables (&optional mode-symbol dont-search-parents)
  "Get snippet tables for current buffer.

Return a list of 'yas/snippet-table' objects indexed by mode.

The modes are tried in this order: optional MODE-SYMBOL, then
`yas/mode-symbol', then `major-mode' then, unless
DONT-SEARCH-PARENTS is non-nil, the guessed parent mode of either
MODE-SYMBOL or `major-mode'.

Guessing is done by looking up the MODE-SYMBOL's
`derived-mode-parent' property, see also `derived-mode-p'."
  (let ((mode-tables
         (mapcar #'(lambda (mode)
                     (gethash mode yas/snippet-tables))
                 (append (list mode-symbol)
                         (if (listp yas/mode-symbol)
                             yas/mode-symbol
                           (list yas/mode-symbol))
                         (list major-mode
                               (and (not dont-search-parents)
                                    (get (or mode-symbol major-mode)
                                         'derived-mode-parent))))))
        (all-tables))
    (dolist (table (remove nil mode-tables))
      (push table all-tables)
      (nconc all-tables (yas/snippet-table-get-all-parents table)))
    (remove-duplicates all-tables)))

(defun yas/menu-keymap-get-create (mode)
  "Get the menu keymap correspondong to MODE."
  (or (gethash mode yas/menu-table)
      (puthash mode (make-sparse-keymap) yas/menu-table)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Template-related and snippet loading functions

(defun yas/parse-template (&optional file)
  "Parse the template in the current buffer.

Optional FILE is the absolute file name of the file being
parsed.

Return a snippet-definition, i.e. a list

 (KEY TEMPLATE NAME CONDITION GROUP VARS FILE KEYBINDING)

If the buffer contains a line of \"# --\" then the contents
above this line are ignored. Variables can be set above this
line through the syntax:

#name : value

Here's a list of currently recognized variables:

 * name
 * contributor
 * condition
 * key
 * group
 * expand-env

#name: #include \"...\"
# --
#include \"$1\""
  ;;
  ;;
  (goto-char (point-min))
  (let* ((name (and file
                    (file-name-nondirectory file)))
         (key (unless yas/ignore-filenames-as-triggers
                (and name
                     (file-name-sans-extension name))))
         template
         bound
         condition
         (group (and file
                     (yas/calculate-group file)))
         expand-env
         binding)
    (if (re-search-forward "^# --\n" nil t)
        (progn (setq template
                     (buffer-substring-no-properties (point)
                                                     (point-max)))
               (setq bound (point))
               (goto-char (point-min))
               (while (re-search-forward "^# *\\([^ ]+?\\) *: *\\(.*\\)$" bound t)
                 (when (string= "name" (match-string-no-properties 1))
                   (setq name (match-string-no-properties 2)))
                 (when (string= "condition" (match-string-no-properties 1))
                   (setq condition (match-string-no-properties 2)))
                 (when (string= "group" (match-string-no-properties 1))
                   (setq group (match-string-no-properties 2)))
                 (when (string= "expand-env" (match-string-no-properties 1))
                   (setq expand-env (match-string-no-properties 2)))
                 (when (string= "key" (match-string-no-properties 1))
                   (setq key (match-string-no-properties 2)))
                 (when (string= "binding" (match-string-no-properties 1))
                   (setq binding (match-string-no-properties 2)))))
      (setq template
            (buffer-substring-no-properties (point-min) (point-max))))
    (list key template name condition group expand-env file binding)))

(defun yas/calculate-group (file)
  "Calculate the group for snippet file path FILE."
  (let* ((dominating-dir (locate-dominating-file file
                                                 ".yas-make-groups"))
         (extra-path (and dominating-dir
                          (replace-regexp-in-string (concat "^"
                                                            (expand-file-name dominating-dir))
                                                    ""
                                                    (expand-file-name file))))
         (extra-dir (and extra-path
                         (file-name-directory extra-path)))
         (group (and extra-dir
                     (replace-regexp-in-string "/"
                                               "."
                                               (directory-file-name extra-dir)))))
    group))

;; (defun yas/glob-files (directory &optional recurse-p append)
;;   "Returns files under DIRECTORY ignoring dirs and hidden files.

;; If RECURSE in non-nil, do that recursively."
;;   (let (ret
;;         (default-directory directory))
;;     (dolist (entry (directory-files "."))
;;       (cond ((or (string-match "^\\."
;;                                (file-name-nondirectory entry))
;;                  (string-match "~$"
;;                                (file-name-nondirectory entry)))
;;              nil)
;;             ((and recurse-p
;;                   (file-directory-p entry))
;;              (setq ret (nconc ret
;;                               (yas/glob-files (expand-file-name entry)
;;                                               recurse-p
;;                                               (if append
;;                                                   (concat append "/" entry)
;;                                                 entry)))))
;;             ((file-directory-p entry)
;;              nil)
;;             (t
;;              (push (if append
;;                        (concat append "/" entry)
;;                      entry) ret))))
;;     ret))

(defun yas/subdirs (directory &optional file?)
  "Return subdirs or files of DIRECTORY according to FILE?."
  (remove-if (lambda (file)
               (or (string-match "^\\."
                                 (file-name-nondirectory file))
                   (string-match "~$"
                                 (file-name-nondirectory file))
                   (if file?
                       (file-directory-p file)
                     (not (file-directory-p file)))))
             (directory-files directory t)))

(defun yas/make-menu-binding (template)
  `(lambda () (interactive) (yas/expand-or-visit-from-menu ,template)))

(defun yas/expand-or-visit-from-menu (template)
  (if yas/visit-from-menu
      (yas/visit-snippet-file-1 template)
    (let ((where (if mark-active
                     (cons (region-beginning) (region-end))
                   (cons (point) (point)))))
      (yas/expand-snippet (yas/template-content template)
                          (car where)
                          (cdr where)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Popping up for keys and templates
;;
(defun yas/prompt-for-template (templates &optional prompt)
  "Interactively choose a template from the list TEMPLATES.

TEMPLATES is a list of `yas/template'."
  (when templates
    (some #'(lambda (fn)
              (funcall fn (or prompt "Choose a snippet: ")
                       templates
                       #'yas/template-name))
          yas/prompt-functions)))

(defun yas/prompt-for-keys (keys &optional prompt)
  "Interactively choose a template key from the list KEYS."
  (when keys
    (some #'(lambda (fn)
              (funcall fn (or prompt "Choose a snippet key: ") keys))
          yas/prompt-functions)))

(defun yas/prompt-for-table (tables &optional prompt)
  (when tables
    (some #'(lambda (fn)
              (funcall fn (or prompt "Choose a snippet table: ")
                       tables
                       #'yas/snippet-table-name))
          yas/prompt-functions)))

(defun yas/x-prompt (prompt choices &optional display-fn)
  (when (and window-system choices)
    (let ((keymap (cons 'keymap
                        (cons
                         prompt
                         (mapcar (lambda (choice)
                                   (list choice
                                         'menu-item
                                         (if display-fn
                                             (funcall display-fn choice)
                                           choice)
                                         t))
                                 choices)))))
      (when (cdr keymap)
        (car (x-popup-menu (if (fboundp 'posn-at-point)
                               (let ((x-y (posn-x-y (posn-at-point (point)))))
                                 (list (list (+ (car x-y) 10)
                                             (+ (cdr x-y) 20))
                                       (selected-window)))
                             t)
                           keymap))))))

(defun yas/ido-prompt (prompt choices &optional display-fn)
  (when (and (featurep 'ido)
             ido-mode)
    (let* ((formatted-choices (or (and display-fn
                                       (mapcar display-fn choices))
                                  choices))
           (chosen (and formatted-choices
                        (ido-completing-read prompt
                                             formatted-choices
                                             nil
                                             'require-match
                                             nil
                                             nil))))
      (when chosen
        (nth (position chosen formatted-choices :test #'string=) choices)))))

(eval-when-compile (require 'dropdown-list nil t))
(defun yas/dropdown-prompt (prompt choices &optional display-fn)
  (when (featurep 'dropdown-list)
    (let* ((formatted-choices (or (and display-fn
                                       (mapcar display-fn choices))
                                  choices))
           (chosen (and formatted-choices
                        (nth (dropdown-list formatted-choices)
                             choices))))
      chosen)))

(defun yas/completing-prompt (prompt choices &optional display-fn)
  (let* ((formatted-choices (or (and display-fn
                                     (mapcar display-fn choices))
                                choices))
         (chosen (and formatted-choices
                      (completing-read prompt
                                       formatted-choices
                                       nil
                                       'require-match
                                       nil
                                       nil))))
    (when chosen
      (nth (position chosen formatted-choices :test #'string=) choices))))

(defun yas/no-prompt (prompt choices &optional display-fn)
  (first choices))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Loading snippets from files
;;
(defun yas/load-directory-1 (directory &optional parents no-hierarchy-parents making-groups-sym)
  "Recursively load snippet templates from DIRECTORY."
  ;; TODO: Rewrite this horrible, horrible monster I created
  (unless (file-exists-p (concat directory "/" ".yas-skip"))
    (let* ((major-mode-and-parents (unless making-groups-sym
                                     (yas/compute-major-mode-and-parents (concat directory "/dummy")
                                                                         nil
                                                                         no-hierarchy-parents)))
           (yas/ignore-filenames-as-triggers (or yas/ignore-filenames-as-triggers
                                                 (file-exists-p (concat directory "/" ".yas-ignore-filenames-as-triggers"))))
           (mode-sym (and major-mode-and-parents
                          (car major-mode-and-parents)))
           (parents (if making-groups-sym
                        parents
                      (rest major-mode-and-parents)))
           (snippet-defs nil)
           (make-groups-p (or making-groups-sym
                              (file-exists-p (concat directory "/" ".yas-make-groups")))))
      (with-temp-buffer
        (dolist (file (yas/subdirs directory 'no-subdirs-just-files))
          (when (file-readable-p file)
            (insert-file-contents file nil nil nil t)
            (push (yas/parse-template file)
                  snippet-defs))))
      (yas/define-snippets (or mode-sym
                               making-groups-sym)
                           snippet-defs
                           parents)
      (dolist (subdir (yas/subdirs directory))
        (if make-groups-p
            (yas/load-directory-1 subdir parents 't (or mode-sym
                                                        making-groups-sym))
          (yas/load-directory-1 subdir (list mode-sym)))))))

(defun yas/load-directory (directory)
  "Load snippet definition from a directory hierarchy.

Below the top-level directory, each directory is a mode
name.  And under each subdirectory, each file is a definition
of a snippet.  The file name is the trigger key and the
content of the file is the template."
  (interactive "DSelect the root directory: ")
  (unless (file-directory-p directory)
    (error "Error %s not a directory" directory))
  (unless yas/root-directory
    (setq yas/root-directory directory))
  (dolist (dir (yas/subdirs directory))
    (yas/load-directory-1 dir nil 'no-hierarchy-parents))
  (when (interactive-p)
    (message "done.")))

(defun yas/kill-snippet-keybindings ()
  "Remove the all active snippet keybindings."
  (interactive)
  (dolist (keybinding yas/active-keybindings)
    (define-key (symbol-value (first keybinding)) (second keybinding) nil))
  (setq yas/active-keybindings nil))

(defun yas/reload-all (&optional reset-root-directory)
  "Reload all snippets and rebuild the YASnippet menu. "
  (interactive "P")
  ;; Turn off global modes and minor modes, save their state though
  ;;
  (let ((restore-global-mode (prog1 yas/global-mode
                               (yas/global-mode -1)))
        (restore-minor-mode (prog1 yas/minor-mode
                              (yas/minor-mode -1))))
    ;; Empty all snippet tables and all menu tables
    ;;
    (setq yas/snippet-tables (make-hash-table))
    (setq yas/menu-table (make-hash-table))

    ;; Init the `yas/minor-mode-map', taking care not to break the
    ;; menu....
    ;;
    (setf (cdr yas/minor-mode-map)
          (cdr (yas/init-minor-keymap)))

    ;; Now, clean up the other keymaps we might have cluttered up.
    (yas/kill-snippet-keybindings)

    (when reset-root-directory
      (setq yas/root-directory nil))

    ;; Reload the directories listed in `yas/root-directory' or prompt
    ;; the user to select one.
    ;;
    (if yas/root-directory
        (if (listp yas/root-directory)
            (dolist (directory yas/root-directory)
              (yas/load-directory directory))
          (yas/load-directory yas/root-directory))
      (call-interactively 'yas/load-directory))

    ;; Restore the mode configuration
    ;;
    (when restore-minor-mode
      (yas/minor-mode 1))
    (when restore-global-mode
      (yas/global-mode 1))

    (message "[yas] Reloading everything... Done.")))

(defun yas/quote-string (string)
  "Escape and quote STRING.
foo\"bar\\! -> \"foo\\\"bar\\\\!\""
  (concat "\""
          (replace-regexp-in-string "[\\\"]"
                                    "\\\\\\&"
                                    string
                                    t)
          "\""))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Yasnippet Bundle

(defun yas/initialize ()
  "For backward compatibility, enable `yas/minor-mode' globally"
  (yas/global-mode 1))

(defun yas/compile-bundle
  (&optional yasnippet yasnippet-bundle snippet-roots code dropdown)
  "Compile snippets in SNIPPET-ROOTS to a single bundle file.

YASNIPPET is the yasnippet.el file path.

YASNIPPET-BUNDLE is the output file of the compile result.

SNIPPET-ROOTS is a list of root directories that contains the
snippets definition.

CODE is the code to be placed at the end of the generated file
and that can initialize the YASnippet bundle.

Last optional argument DROPDOWN is the filename of the
dropdown-list.el library.

Here's the default value for all the parameters:

  (yas/compile-bundle \"yasnippet.el\"
                      \"yasnippet-bundle.el\"
                      \"snippets\")
                      \"(yas/initialize-bundle)
                        ### autoload
                        (require 'yasnippet-bundle)`\"
                      \"dropdown-list.el\")
"
  (interactive "ffind the yasnippet.el file: \nFTarget bundle file: \nDSnippet directory to bundle: \nMExtra code? \nfdropdown-library: ")
  
  (let* ((yasnippet (or yasnippet
                        "yasnippet.el"))
         (yasnippet-bundle (or yasnippet-bundle
                               "./yasnippet-bundle.el"))
         (snippet-roots (or snippet-roots
                            "snippets"))
         (dropdown (or dropdown
                       "dropdown-list.el"))
         (code (or (and code
                        (condition-case err (read code) (error nil))
                        code)
                   (concat "(yas/initialize-bundle)"
                           "\n;;;###autoload" ; break through so that won't
                           "(require 'yasnippet-bundle)")))
         (dirs (or (and (listp snippet-roots) snippet-roots)
                   (list snippet-roots)))
         (bundle-buffer nil))
    (with-temp-file yasnippet-bundle
      (insert ";;; yasnippet-bundle.el --- "
              "Yet another snippet extension (Auto compiled bundle)\n")
      (insert-file-contents yasnippet)
      (goto-char (point-max))
      (insert "\n")
      (when dropdown
        (insert-file-contents dropdown))
      (goto-char (point-max))
      (insert ";;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;\n")
      (insert ";;;;      Auto-generated code         ;;;;\n")
      (insert ";;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;\n")
      (insert "(defun yas/initialize-bundle ()\n"
              "  \"Initialize YASnippet and load snippets in the bundle.\"")
      (flet ((yas/define-snippets
              (mode snippets &optional parent-or-parents)
              (insert ";;; snippets for " (symbol-name mode) "\n")
              (let ((literal-snippets (list)))
                (dolist (snippet snippets)
                  (let ((key                    (first   snippet))
                        (template-content       (second  snippet))
                        (name                   (third   snippet))
                        (condition              (fourth  snippet))
                        (group                  (fifth   snippet))
                        (expand-env             (sixth   snippet))
                        ;; Omit the file on purpose
                        (file                   nil) ;; (seventh snippet)) 
                        (binding                (eighth  snippet)))
                    (push `(,key
                            ,template-content
                            ,name
                            ,condition
                            ,group
                            ,expand-env
                            ,file
                            ,binding)
                          literal-snippets)))
                (insert (pp-to-string `(yas/define-snippets ',mode ',literal-snippets ',parent-or-parents)))
                (insert "\n\n"))))
        (dolist (dir dirs)
          (dolist (subdir (yas/subdirs dir))
            (yas/load-directory-1 subdir nil 'no-hierarchy-parents))))

      (insert (pp-to-string `(yas/global-mode 1)))
      (insert ")\n\n" code "\n")

      ;; bundle-specific provide and value for yas/dont-activate
      (let ((bundle-feature-name (file-name-nondirectory
                                  (file-name-sans-extension
                                   yasnippet-bundle)))) 
        (insert (pp-to-string `(set-default 'yas/dont-activate
                                            #'(lambda ()
                                                (and (or yas/root-directory
                                                         (featurep ',(make-symbol bundle-feature-name)))
                                                     (null (yas/get-snippet-tables)))))))
        (insert (pp-to-string `(provide ',(make-symbol bundle-feature-name)))))
      
      (insert ";;; "
              (file-name-nondirectory yasnippet-bundle)
              " ends here\n"))))

(defun yas/compile-textmate-bundle ()
  (interactive)
  (yas/compile-bundle "yasnippet.el"
                      "./yasnippet-textmate-bundle.el"
                      "extras/imported/"
                      (concat "(yas/initialize-bundle)"
                              "\n;;;###autoload" ; break through so that won't
                              "(require 'yasnippet-textmate-bundle)")
                      "dropdown-list.el"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Some user level functions
;;;

(defun yas/about ()
  (interactive)
  (message (concat "yasnippet (version "
                   yas/version
                   ") -- pluskid <pluskid@gmail.com>/joaotavora <joaotavora@gmail.com>")))

(defun yas/define-snippets (mode snippets &optional parent-mode)
  "Define SNIPPETS for MODE.

SNIPPETS is a list of snippet definitions, each taking the
following form:

 (KEY TEMPLATE NAME CONDITION GROUP EXPAND-ENV FILE KEYBINDING)

Within these, only TEMPLATE is actually mandatory.

All the elelements are strings, including CONDITION, EXPAND-ENV
and KEYBINDING which will be `read' and eventually `eval'-ed.

FILE is probably of very little use if you're programatically
defining snippets.

You can use `yas/parse-template' to return such lists based on
the current buffers contents.

Optional PARENT-MODE can be used to specify the parent tables of
MODE. It can be a mode symbol of a list of mode symbols. It does
not need to be a real mode."
  (let ((snippet-table (yas/snippet-table-get-create mode))
        (parent-tables (mapcar #'yas/snippet-table-get-create
                               (if (listp parent-mode)
                                   parent-mode
                                 (list parent-mode))))
        (keymap (if yas/use-menu
                    (yas/menu-keymap-get-create mode)
                  nil)))
    ;; Setup the menu
    ;;
    (when parent-tables
      (setf (yas/snippet-table-parents snippet-table)
            parent-tables)
      (when yas/use-menu
        (let ((parent-menu-syms-and-names
               (if (listp parent-mode)
                   (mapcar #'(lambda (sym)
                               (cons sym (concat "parent mode - " (symbol-name sym))))
                           parent-mode)
                 '((parent-mode . "parent mode")))))
          (mapc #'(lambda (sym-and-name)
                    (define-key keymap
                      (vector (intern (replace-regexp-in-string " " "_" (cdr sym-and-name))))
                      (list 'menu-item (cdr sym-and-name)
                            (yas/menu-keymap-get-create (car sym-and-name)))))
                (reverse parent-menu-syms-and-names)))))
    (when yas/use-menu
      (define-key yas/minor-mode-menu (vector mode)
        `(menu-item ,(symbol-name mode) ,keymap
                    :visible (yas/show-menu-p ',mode))))
    ;; Iterate the recently parsed snippets definition
    ;;
    (dolist (snippet snippets)
      (let* ((file (seventh snippet))
             (key (or (car snippet)
                      (unless yas/ignore-filenames-as-triggers
                        (and file 
                             (file-name-sans-extension (file-name-nondirectory file))))))
             (name (or (third snippet)
                       (and file
                            (file-name-directory file))))
             (condition (fourth snippet))
             (group (fifth snippet))
             (keybinding (eighth snippet))
             (template nil))
        ;; Read the snippet's "binding :" expression
        ;;
        (condition-case err
            (when keybinding
              (setq keybinding (read (eighth snippet)))
              (let* ((this-mode-map-symbol (intern (concat (symbol-name mode) "-map")))
                     (keys (or (and (consp keybinding)
                                    (read-kbd-macro (cdr keybinding)))
                               (read-kbd-macro keybinding)))
                     (keymap-symbol (or (and (consp keybinding)
                                             (car keybinding))
                                        this-mode-map-symbol)))
                (if (and (boundp keymap-symbol)
                         (keymapp (symbol-value keymap-symbol)))
                    (setq keybinding (list keymap-symbol
                                           keys
                                           name))
                  (error (format "keymap \"%s\" does not (yet?) exist" keymap-symbol)))))
          (error
           (message "[yas] warning: keybinding \"%s\" invalid for snippet \"%s\" since %s."
                    keybinding name (error-message-string err))
           (setf keybinding nil)))

        ;; Create the `yas/template' object and store in the
        ;; appropriate snippet table. This only done if we have found
        ;; a key and a name for the snippet, because that is what
        ;; indexes the snippet tables
        ;;
        (setq template (yas/make-template (second snippet)
                                          (or name key)
                                          condition
                                          (sixth snippet)
                                          (seventh snippet)
                                          keybinding))
        (when (and key
                   name)
          (yas/store snippet-table
                     name
                     key
                     template))
        ;; If we have a keybinding, register it if it does not
        ;; conflict!
        ;;
        (when keybinding
	  (let ((lookup (lookup-key (symbol-value (first keybinding)) (second keybinding))))
	    (if (and lookup
		     (not (numberp lookup)))
		(message "[yas] warning: won't overwrite keybinding \"%s\" for snippet \"%s\" in `%s'"
			 (key-description (second keybinding)) name (first keybinding))
	      (define-key
		(symbol-value (first keybinding))
		(second keybinding)
		`(lambda (&optional yas/prefix)
		   (interactive "P")
		   (when (yas/template-can-expand-p ,(yas/template-condition template))
		     (yas/expand-snippet ,(yas/template-content template)
					 nil
					 nil
					 ,(yas/template-expand-env template)))))
	      (add-to-list 'yas/active-keybindings keybinding))))

        ;; Setup the menu groups, reorganizing from group to group if
        ;; necessary
        ;;
        (when yas/use-menu
          (let ((group-keymap keymap))
            ;; Delete this entry from another group if already exists
            ;; in some other group. An entry is considered as existing
            ;; in another group if its name string-matches.
            ;;
            (yas/delete-from-keymap group-keymap name)

            ;; ... then add this entry to the correct group
            (when (and (not (null group))
                       (not (string= "" group)))
              (dolist (subgroup (mapcar #'make-symbol
                                        (split-string group "\\.")))
                (let ((subgroup-keymap (lookup-key group-keymap
                                                   (vector subgroup))))
                  (when (null subgroup-keymap)
                    (setq subgroup-keymap (make-sparse-keymap))
                    (define-key group-keymap (vector subgroup)
                      `(menu-item ,(symbol-name subgroup)
                                  ,subgroup-keymap)))
                  (setq group-keymap subgroup-keymap))))
            (define-key group-keymap (vector (gensym))
              `(menu-item ,(yas/template-name template)
                          ,(yas/make-menu-binding template)
                          :help ,name
                          :keys ,(when (and key name)
                                   (concat key yas/trigger-symbol))))))))))

(defun yas/show-menu-p (mode)
  (cond ((eq yas/use-menu 'abbreviate)
         (find mode
               (mapcar #'(lambda (table)
                           (intern (yas/snippet-table-name table)))
                       (yas/get-snippet-tables))))
        ((eq yas/use-menu 'real-modes)
         (yas/real-mode? mode))
        (t
         t))) 

(defun yas/delete-from-keymap (keymap name)
  "Recursively delete items name NAME from KEYMAP and its submenus.

Skip any submenus named \"parent mode\""
  ;; First of all, recursively enter submenus, i.e. the tree is
  ;; searched depth first so that stale submenus can be found in the
  ;; higher passes.
  ;;
  (mapc #'(lambda (item)
            (when (and (keymapp (fourth item))
                       (stringp (third item))
                       (not (string-match "parent mode" (third item))))
              (yas/delete-from-keymap (fourth item) name)))
        (rest keymap))
  ;;
  (when (keymapp keymap)
    (let ((pos-in-keymap))
      (while (setq pos-in-keymap
                   (position-if #'(lambda (item)
                                    (and (listp item)
                                         (or
                                          ;; the menu item we want to delete
                                          (and (eq 'menu-item (second item))
                                               (third item)
                                               (and (string= (third item) name)))
                                          ;; a stale subgroup
                                          (and (keymapp (fourth item))
                                               (not (and (stringp (third item))
                                                         (string-match "parent mode"
                                                                       (third item))))
                                               (null (rest (fourth item)))))))
                                keymap))
        (setf (nthcdr pos-in-keymap keymap)
              (nthcdr (+ 1 pos-in-keymap) keymap))))))

(defun yas/define (mode key template &optional name condition group)
  "Define a snippet.  Expanding KEY into TEMPLATE.

NAME is a description to this template.  Also update the menu if
`yas/use-menu' is `t'.  CONDITION is the condition attached to
this snippet.  If you attach a condition to a snippet, then it
will only be expanded when the condition evaluated to non-nil."
  (yas/define-snippets mode
                       (list (list key template name condition group))))

(defun yas/hippie-try-expand (first-time?)
  "Integrate with hippie expand.  Just put this function in
`hippie-expand-try-functions-list'."
  (if (not first-time?)
      (let ((yas/fallback-behavior 'return-nil))
        (yas/expand))
    (undo 1)
    nil))

(defun yas/expand ()
  "Expand a snippet before point.

If no snippet expansion is possible, fall back to the behaviour
defined in `yas/fallback-behavior'"
  (interactive)
  (yas/expand-1))

(defun yas/expand-1 (&optional field)
  "Actually fo the work for `yas/expand'"
  (multiple-value-bind (templates start end) (if field
                                                 (save-restriction
                                                   (narrow-to-region (yas/field-start field) (yas/field-end field))
                                                   (yas/current-key))
                                               (yas/current-key))
    (if templates
        (let ((template (or (and (rest templates) ;; more than one
                                 (yas/prompt-for-template (mapcar #'cdr templates)))
                            (cdar templates))))
          (when template
            (yas/expand-snippet (yas/template-content template)
                                start
                                end
                                (yas/template-expand-env template))))
      (cond ((eq yas/fallback-behavior 'return-nil)
             ;; return nil
             nil)
            ((eq yas/fallback-behavior 'call-other-command)
             (let* ((yas/minor-mode nil)
                    (keys-1 (this-command-keys-vector))
                    (keys-2 (and yas/trigger-key
                                 (stringp yas/trigger-key)
                                 (read-kbd-macro yas/trigger-key))) 
                    (command-1 (and keys-1 (key-binding keys-1)))
                    (command-2 (and keys-2 (key-binding keys-2)))
                    (command (or (and (not (eq command-1 'yas/expand))
                                      command-1)
                                 command-2)))
               (when (and (commandp command)
                          (not (eq 'yas/expand command)))
                 (setq this-command command)
                 (call-interactively command))))
            ((and (listp yas/fallback-behavior)
                  (cdr yas/fallback-behavior)
                  (eq 'apply (car yas/fallback-behavior)))
             (if (cddr yas/fallback-behavior)
                 (apply (cadr yas/fallback-behavior)
                        (cddr yas/fallback-behavior))
               (when (commandp (cadr yas/fallback-behavior))
                 (setq this-command (cadr yas/fallback-behavior))
                 (call-interactively (cadr yas/fallback-behavior)))))
            (t
             ;; also return nil if all the other fallbacks have failed
             nil)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Snippet development

(defun yas/all-templates (tables)
  "Return all snippet tables applicable for the current buffer.

Honours `yas/choose-tables-first', `yas/choose-keys-first' and
`yas/buffer-local-condition'"
  (when yas/choose-tables-first
    (setq tables (list (yas/prompt-for-table tables))))
  (mapcar #'cdr
          (if yas/choose-keys-first
              (let ((key (yas/prompt-for-keys
                          (mapcan #'yas/snippet-table-all-keys tables))))
                (when key
                  (mapcan #'(lambda (table)
                              (yas/fetch table key))
                          tables)))
            (mapcan #'yas/snippet-table-templates tables))))

(defun yas/insert-snippet (&optional no-condition)
  "Choose a snippet to expand, pop-up a list of choices according
to `yas/prompt-function'.

With prefix argument NO-CONDITION, bypass filtering of snippets
by condition."
  (interactive "P")
  (let* ((yas/buffer-local-condition (or (and no-condition
                                              'always)
                                         yas/buffer-local-condition))
         (templates (yas/all-templates (yas/get-snippet-tables)))
         (template (and templates
                        (or (and (rest templates) ;; more than one template for same key
                                 (yas/prompt-for-template templates))
                            (car templates))))
         (where (if mark-active
                    (cons (region-beginning) (region-end))
                  (cons (point) (point)))))
    (if template
        (yas/expand-snippet (yas/template-content template)
                            (car where)
                            (cdr where)
                            (yas/template-expand-env template))
      (message "[yas] No snippets can be inserted here!"))))

(defun yas/visit-snippet-file ()
  "Choose a snippet to edit, selection like `yas/insert-snippet'.

Only success if selected snippet was loaded from a file.  Put the
visited file in `snippet-mode'."
  (interactive)
  (let* ((yas/buffer-local-condition 'always)
         (templates (yas/all-templates (yas/get-snippet-tables)))
         (template (and templates
                        (or (and (rest templates) ;; more than one template for same key
                                 (yas/prompt-for-template templates
                                                          "Choose a snippet template to edit: "))
                            (car templates)))))

    (when template
      (yas/visit-snippet-file-1 template))))

(defun yas/visit-snippet-file-1 (template)
  (let ((file (yas/template-file template)))
    (cond ((and file (file-exists-p file))
           (find-file-other-window file)
           (snippet-mode))
          (file
           (message "Original file %s no longer exists!" file))
          (t
           (message "This snippet was not loaded from a file!")))))

(defun yas/guess-snippet-directories-1 (table &optional suffix)
  "Guesses possible snippet subdirsdirectories for TABLE."
  (unless suffix
    (setq suffix (yas/snippet-table-name table))) 
  (cons suffix
        (mapcan #'(lambda (parent)
                    (yas/guess-snippet-directories-1
                     parent
                     (concat (yas/snippet-table-name parent) "/" suffix)))
                (yas/snippet-table-parents table))))

(defun yas/guess-snippet-directories ()
  "Try to guess suitable directories based on the current active
tables.

Returns a a list of options alist TABLE -> DIRS where DIRS are
all the possibly directories where snippets of table might be
lurking."
  (let ((main-dir (or (and (listp yas/root-directory)
                           (first yas/root-directory))
                      yas/root-directory
                      (setq yas/root-directory "~/.emacs.d/snippets")))
        (tables (yas/get-snippet-tables)))
    ;; HACK! the snippet table created here is a dummy table that
    ;; holds the correct name so that `yas/make-directory-maybe' can
    ;; work. The real table, if it does not exist in
    ;; yas/snippet-tables will be created when the first snippet for
    ;; that mode is loaded.
    ;; 
    (unless (gethash major-mode yas/snippet-tables)
      (setq tables (cons (yas/make-snippet-table (symbol-name major-mode))
                         tables)))
    
    (mapcar #'(lambda (table)
                (cons table
                      (mapcar #'(lambda (subdir)
                                  (concat main-dir "/" subdir))
                              (yas/guess-snippet-directories-1 table))))
            tables)))

(defun yas/make-directory-maybe (table-and-dirs &optional main-table-string)
  "Returns a dir inside  TABLE-AND-DIRS, prompts for creation if none exists."
  (or (some #'(lambda (dir) (when (file-directory-p dir) dir)) (cdr table-and-dirs))
      (let ((candidate (first (cdr table-and-dirs))))
        (if (y-or-n-p (format "Guessed directory (%s) for%s%s table \"%s\" does not exist! Create? "
                              candidate
                              (if (gethash (intern (yas/snippet-table-name (car table-and-dirs)))
                                           yas/snippet-tables)
                                  ""
                                " brand new")
                              (or main-table-string
                                  "")
                              (yas/snippet-table-name (car table-and-dirs))))
            (progn
              (make-directory candidate 'also-make-parents)
              ;; create the .yas-parents file here...
              candidate)))))

(defun yas/new-snippet (&optional choose-instead-of-guess)
  ""
  (interactive "P")
  (let* ((guessed-directories (yas/guess-snippet-directories))
         (option (or (and choose-instead-of-guess
                          (some #'(lambda (fn)
                                    (funcall fn "Choose a snippet table: "
                                             guessed-directories
                                             #'(lambda (option)
                                                 (yas/snippet-table-name (car option)))))
                                yas/prompt-functions))
                     (first guessed-directories)))
         (chosen))
    (setq chosen (yas/make-directory-maybe option (unless choose-instead-of-guess
                                                    " main")))
    (unless (or chosen
                choose-instead-of-guess)
      (if (y-or-n-p (format "Continue guessing for other active tables %s? "
                            (mapcar #'(lambda (table-and-dirs)
                                        (yas/snippet-table-name (car table-and-dirs)))
                                    (rest guessed-directories))))
          (setq chosen (some #'yas/make-directory-maybe
                             (rest guessed-directories)))))
    (unless (or chosen
                choose-instead-of-guess)
      (when (y-or-n-p "Having trouble... use snippet root dir? ")
        (setq chosen (if (listp yas/root-directory)
                         (first yas/root-directory)
                       yas/root-directory))))
    (if chosen
        (let ((default-directory chosen)
              (name (read-from-minibuffer "Enter a snippet name: ")))
          (find-file-other-window (concat name
                                          ".yasnippet"))
          (snippet-mode)
          (unless (and choose-instead-of-guess
                       (not (y-or-n-p "Insert a snippet with useful headers? ")))
            (yas/expand-snippet (format 
                                 "\
# -*- mode: snippet -*-
# name: %s
# key: $1${2:
# binding: \"${3:keybinding}\"}${4:
# expand-env: ((${5:some-var} ${6:some-value}))}
# --
$0" name))))
      (message "[yas] aborted snippet creation."))))

(defun yas/find-snippets (&optional same-window )
  "Look for user snippets in guessed current mode's directory.

Calls `find-file' interactively in the guessed directory.

With prefix arg SAME-WINDOW opens the buffer in the same window.

Because snippets can be loaded from many different locations,
this has to guess the correct directory using
`yas/guess-snippet-directories', which returns a list of
options. 

If any one of these exists, it is taken and `find-file' is called
there, otherwise, proposes to create the first option returned by
`yas/guess-snippet-directories'."
  (interactive "P")
  (let* ((guessed-directories (yas/guess-snippet-directories))
         (chosen)
         (buffer))
    (setq chosen (yas/make-directory-maybe (first guessed-directories) " main"))
    (unless chosen
      (if (y-or-n-p (format "Continue guessing for other active tables %s? "
                            (mapcar #'(lambda (table-and-dirs)
                                        (yas/snippet-table-name (car table-and-dirs)))
                                    (rest guessed-directories))))
          (setq chosen (some #'yas/make-directory-maybe
                             (rest guessed-directories)))))
    (unless chosen
      (when (y-or-n-p "Having trouble... go to snippet root dir? ")
        (setq chosen (if (listp yas/root-directory)
                         (first yas/root-directory)
                       yas/root-directory))))
    (if chosen
        (let ((default-directory chosen))
          (setq buffer (call-interactively (if same-window
                                               'find-file
                                             'find-file-other-window)))
          (when buffer
            (save-excursion
              (set-buffer buffer)
              (when (eq major-mode 'fundamental-mode)
                (snippet-mode)))))
      (message "Could not guess snippet dir!"))))

(defun yas/compute-major-mode-and-parents (file &optional prompt-if-failed no-hierarchy-parents)
  (let* ((file-dir (and file
                        (directory-file-name (or (locate-dominating-file file ".yas-make-groups")
                                                 (directory-file-name (file-name-directory file))))))
         (major-mode-name (and file-dir
                               (file-name-nondirectory file-dir)))
         (parent-file-dir (and file-dir
                               (directory-file-name (file-name-directory file-dir))))
         (parent-mode-name (and parent-file-dir
                                (not no-hierarchy-parents)
                                (file-name-nondirectory parent-file-dir)))
         (major-mode-sym (or (and major-mode-name
                                  (intern major-mode-name))
                             (when prompt-if-failed
                               (read-from-minibuffer
                                "[yas] Cannot auto-detect major mode! Enter a major mode: "))))
         (parent-mode-sym (and parent-mode-name
                               (intern parent-mode-name)))
         (extra-parents-file-name (concat file-dir "/.yas-parents"))
         (more-parents (when (file-readable-p extra-parents-file-name)
                         (mapcar #'intern
                                 (split-string
                                  (with-temp-buffer
                                    (insert-file-contents extra-parents-file-name)
                                    (buffer-substring-no-properties (point-min)
                                                                    (point-max))))))))
    (when major-mode-sym
      (remove nil (append (list major-mode-sym parent-mode-sym)
                          more-parents)))))

(defun yas/load-snippet-buffer (&optional kill)
  "Parse and load current buffer's snippet definition.

With optional prefix argument KILL quit the window and buffer."
  (interactive "P")
  (if buffer-file-name
      (let ((major-mode-and-parent (yas/compute-major-mode-and-parents buffer-file-name)))
        (if major-mode-and-parent
            (let* ((parsed (yas/parse-template buffer-file-name))
                   (name (and parsed
                              (third parsed))))
              (when name
                (let ((yas/better-guess-for-replacements t))
                  (yas/define-snippets (car major-mode-and-parent)
                                       (list parsed)
                                       (cdr major-mode-and-parent)))
                (when (and (buffer-modified-p)
                           (y-or-n-p "Save snippet? "))
                  (save-buffer))
                (if kill
                    (quit-window kill)
                  (message "[yas] Snippet \"%s\" loaded for %s."
                           name
                           (car major-mode-and-parent)))))
          (message "[yas] Cannot load snippet for unknown major mode")))
    (message "Save the buffer as a file first!")))

(defun yas/tryout-snippet (&optional debug)
  "Test current buffers's snippet template in other buffer."
  (interactive "P")
  (let* ((major-mode-and-parent (yas/compute-major-mode-and-parents buffer-file-name))
         (parsed (yas/parse-template))
         (test-mode (or (and (car major-mode-and-parent)
                             (fboundp (car major-mode-and-parent))
                             (car major-mode-and-parent))
                        (intern (read-from-minibuffer "[yas] please input a mode: "))))
         (template (and parsed
                        (fboundp test-mode)
                        (yas/make-template (second parsed)
                                           (third parsed)
                                           nil
                                           (sixth parsed)
                                           nil
                                           nil))))
    (cond (template
           (let ((buffer-name (format "*YAS TEST: %s*" (yas/template-name template))))
             (set-buffer (switch-to-buffer buffer-name))
             (erase-buffer)
             (setq buffer-undo-list nil)
             (funcall test-mode)
             (yas/expand-snippet (yas/template-content template)
                                 (point-min)
                                 (point-max)
                                 (yas/template-expand-env template))
             (when debug
               (add-hook 'post-command-hook 'yas/debug-snippet-vars 't 'local))))
          (t
           (message "[yas] Cannot test snippet for unknown major mode")))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; User convenience functions, for using in snippet definitions

(defvar yas/modified-p nil
  "Non-nil if field has been modified by user or transformation.")

(defvar yas/moving-away-p nil
  "Non-nil if user is about to exit field.")

(defvar yas/text nil
  "Contains current field text.")

(defun yas/substr (str pattern &optional subexp)
  "Search PATTERN in STR and return SUBEXPth match.

If found, the content of subexp group SUBEXP (default 0) is
  returned, or else the original STR will be returned."
  (let ((grp (or subexp 0)))
    (save-match-data
      (if (string-match pattern str)
          (match-string-no-properties grp str)
        str))))

(defun yas/choose-value (possibilities)
  "Prompt for a string in the list POSSIBILITIES and return it."
  (unless (or yas/moving-away-p
              yas/modified-p)
    (some #'(lambda (fn)
              (funcall fn "Choose: " possibilities))
          yas/prompt-functions)))

(defun yas/key-to-value (alist)
  "Prompt for a string in the list POSSIBILITIES and return it."
  (unless (or yas/moving-away-p
              yas/modified-p)
    (let ((key (read-key-sequence "")))
      (when (stringp key)
        (or (cdr (find key alist :key #'car :test #'string=))
            key)))))

(defun yas/throw (text)
  "Throw a yas/exception with TEXT as the reason."
  (throw 'yas/exception (cons 'yas/exception text)))

(defun yas/verify-value (possibilities)
  "Verify that the current field value is in POSSIBILITIES

Otherwise throw exception."
  (when (and yas/moving-away-p (notany #'(lambda (pos) (string= pos yas/text)) possibilities))
    (yas/throw (format "[yas] field only allows %s" possibilities))))

(defun yas/field-value (number)
  (let* ((snippet (car (yas/snippets-at-point)))
         (field (and snippet
                     (yas/snippet-find-field snippet number))))
    (when field
      (yas/field-text-for-display field))))

(defun yas/default-from-field (number)
  (unless yas/modified-p
    (yas/field-value number)))

(defun yas/inside-string ()
  (equal 'font-lock-string-face (get-char-property (1- (point)) 'face)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Snippet expansion and field management

(defvar yas/active-field-overlay nil
  "Overlays the currently active field.")

(defvar yas/field-protection-overlays nil
  "Two overlays protect the current active field ")

(defconst yas/prefix nil
  "A prefix argument for expansion direct from keybindings")

(defvar yas/deleted-text nil
  "The text deleted in the last snippet expansion.")

(defvar yas/selected-text nil
  "The selected region deleted on the last snippet expansion.")

(defvar yas/start-column nil
  "The column where the snippet expansion started.")

(make-variable-buffer-local 'yas/active-field-overlay)
(make-variable-buffer-local 'yas/field-protection-overlays)
(make-variable-buffer-local 'yas/deleted-text)

(defstruct (yas/snippet (:constructor yas/make-snippet ()))
  "A snippet.

..."
  (fields '())
  (exit nil)
  (id (yas/snippet-next-id) :read-only t)
  (control-overlay nil)
  active-field
  ;; stacked expansion: the `previous-active-field' slot saves the
  ;; active field where the child expansion took place
  previous-active-field
  force-exit)

(defstruct (yas/field (:constructor yas/make-field (number start end parent-field)))
  "A field."
  number
  start end
  parent-field
  (mirrors '())
  (transform nil)
  (modified-p nil)
  next)

(defstruct (yas/mirror (:constructor yas/make-mirror (start end transform)))
  "A mirror."
  start end
  (transform nil)
  next)

(defstruct (yas/exit (:constructor yas/make-exit (marker)))
  marker
  next)

(defun yas/apply-transform (field-or-mirror field)
  "Calculate the value of the field/mirror. If there's a transform
for this field, apply it. Otherwise, returned nil."
  (let* ((yas/text (yas/field-text-for-display field))
         (text yas/text)
         (yas/modified-p (yas/field-modified-p field))
         (yas/moving-away-p nil)
         (transform (if (yas/mirror-p field-or-mirror)
                        (yas/mirror-transform field-or-mirror)
                      (yas/field-transform field-or-mirror)))
         (start-point (if (yas/mirror-p field-or-mirror)
                          (yas/mirror-start field-or-mirror)
                        (yas/field-start field-or-mirror)))
         (transformed (and transform
                           (save-excursion
                             (goto-char start-point)
                             (yas/read-and-eval-string transform)))))
    transformed))

(defsubst yas/replace-all (from to &optional text)
  "Replace all occurance from FROM to TO.

With optional string TEXT do it in that string."
  (if text
      (replace-regexp-in-string (regexp-quote from) to text t t)
    (goto-char (point-min))
    (while (search-forward from nil t)
      (replace-match to t t text))))

(defun yas/snippet-find-field (snippet number)
  (find-if #'(lambda (field)
               (eq number (yas/field-number field)))
           (yas/snippet-fields snippet)))

(defun yas/snippet-sort-fields (snippet)
  "Sort the fields of SNIPPET in navigation order."
  (setf (yas/snippet-fields snippet)
        (sort (yas/snippet-fields snippet)
              '(lambda (field1 field2)
                 (yas/snippet-field-compare field1 field2)))))

(defun yas/snippet-field-compare (field1 field2)
  "Compare two fields. The field with a number is sorted first.
If they both have a number, compare through the number. If neither
have, compare through the field's start point"
  (let ((n1 (yas/field-number field1))
        (n2 (yas/field-number field2)))
    (if n1
        (if n2
            (< n1 n2)
          t)
      (if n2
          nil
        (< (yas/field-start field1)
           (yas/field-start field2))))))

(defun yas/field-probably-deleted-p (snippet field)
  "Guess if SNIPPET's FIELD should be skipped."
  (and (zerop (- (yas/field-start field) (yas/field-end field)))
       (or (yas/field-parent-field field)
           (and (eq field (car (last (yas/snippet-fields snippet))))
                (= (yas/field-start field) (overlay-end (yas/snippet-control-overlay snippet)))))))

(defun yas/snippets-at-point (&optional all-snippets)
  "Return a sorted list of snippets at point, most recently
inserted first."
  (sort
   (remove nil (remove-duplicates (mapcar #'(lambda (ov)
                                              (overlay-get ov 'yas/snippet))
                                          (if all-snippets
                                              (overlays-in (point-min) (point-max))
                                            (overlays-at (point))))))
   #'(lambda (s1 s2)
       (<= (yas/snippet-id s2) (yas/snippet-id s1)))))

(defun yas/next-field-or-maybe-expand ()
  "Try to expand a snippet at a key before point, otherwise
delegate to `yas/next-field'."
  (interactive)
  (if yas/triggers-in-field
      (let ((yas/fallback-behavior 'return-nil)
            (active-field (overlay-get yas/active-field-overlay 'yas/field)))
        (when active-field
          (unless (yas/expand-1 active-field)
            (yas/next-field))))
    (yas/next-field)))

(defun yas/next-field (&optional arg)
  "Navigate to next field.  If there's none, exit the snippet."
  (interactive)
  (let* ((arg (or arg
                  1))
         (snippet (first (yas/snippets-at-point)))
         (active-field (overlay-get yas/active-field-overlay 'yas/field))
         (live-fields (remove-if #'(lambda (field)
                                     (and (not (eq field active-field))
                                          (yas/field-probably-deleted-p snippet field)))
                                 (yas/snippet-fields snippet)))
         (active-field-pos (position active-field live-fields))
         (target-pos (and active-field-pos (+ arg active-field-pos)))
         (target-field (nth target-pos live-fields)))
    ;; First check if we're moving out of a field with a transform
    ;;
    (when (and active-field
               (yas/field-transform active-field))
      (let* ((yas/moving-away-p t)
             (yas/text (yas/field-text-for-display active-field))
             (text yas/text)
             (yas/modified-p (yas/field-modified-p active-field)))
        ;; primary field transform: exit call to field-transform
        (yas/read-and-eval-string (yas/field-transform active-field))))
    ;; Now actually move...
    (cond ((>= target-pos (length live-fields))
           (yas/exit-snippet snippet))
          (target-field
           (yas/move-to-field snippet target-field))
          (t
           nil))))

(defun yas/place-overlays (snippet field)
  "Correctly place overlays for SNIPPET's FIELD"
  (yas/make-move-field-protection-overlays snippet field)
  (yas/make-move-active-field-overlay snippet field))

(defun yas/move-to-field (snippet field)
  "Update SNIPPET to move to field FIELD.

Also create some protection overlays"
  (goto-char (yas/field-start field))
  (setf (yas/snippet-active-field snippet) field)
  (yas/place-overlays snippet field)
  (overlay-put yas/active-field-overlay 'yas/field field)
  ;; primary field transform: first call to snippet transform
  (unless (yas/field-modified-p field)
    (if (yas/field-update-display field snippet)
        (let ((inhibit-modification-hooks t))
          (yas/update-mirrors snippet))
      (setf (yas/field-modified-p field) nil))))

(defun yas/prev-field ()
  "Navigate to prev field.  If there's none, exit the snippet."
  (interactive)
  (yas/next-field -1))

(defun yas/abort-snippet (&optional snippet)
  (interactive)
  (let ((snippet (or snippet
                     (car (yas/snippets-at-point)))))
    (when snippet
      (setf (yas/snippet-force-exit snippet) t))))

(defun yas/exit-snippet (snippet)
  "Goto exit-marker of SNIPPET."
  (interactive)
  (setf (yas/snippet-force-exit snippet) t)
  (goto-char (if (yas/snippet-exit snippet)
                 (yas/exit-marker (yas/snippet-exit snippet))
               (overlay-end (yas/snippet-control-overlay snippet)))))

(defun yas/exit-all-snippets ()
  "Exit all snippets."
  (interactive)
  (mapc #'(lambda (snippet)
            (yas/exit-snippet snippet)
            (yas/check-commit-snippet))
        (yas/snippets-at-point)))


;;; Apropos markers-to-points:
;;;
;;; This was found useful for performance reasons, so that an
;;; excessive number of live markers aren't kept around in the
;;; `buffer-undo-list'. However, in `markers-to-points', the
;;; set-to-nil markers can't simply be discarded and replaced with
;;; fresh ones in `points-to-markers'. The original marker that was
;;; just set to nil has to be reused.
;;;
;;; This shouldn't bring horrible problems with undo/redo, but it
;;; you never know
;;;

(defun yas/markers-to-points (snippet)
  "Convert all markers in SNIPPET to a cons (POINT . MARKER)
where POINT is the original position of the marker and MARKER is
the original marker object with the position set to nil."
  (dolist (field (yas/snippet-fields snippet))
    (let ((start (marker-position (yas/field-start field)))
          (end (marker-position (yas/field-end field))))
      (set-marker (yas/field-start field) nil)
      (set-marker (yas/field-end field) nil)
      (setf (yas/field-start field) (cons start (yas/field-start field)))
      (setf (yas/field-end field) (cons end (yas/field-end field))))
    (dolist (mirror (yas/field-mirrors field))
      (let ((start (marker-position (yas/mirror-start mirror)))
            (end (marker-position (yas/mirror-end mirror))))
        (set-marker (yas/mirror-start mirror) nil)
        (set-marker (yas/mirror-end mirror) nil)
        (setf (yas/mirror-start mirror) (cons start (yas/mirror-start mirror)))
        (setf (yas/mirror-end mirror) (cons end (yas/mirror-end mirror))))))
  (let ((snippet-exit (yas/snippet-exit snippet)))
    (when snippet-exit
      (let ((exit (marker-position (yas/exit-marker snippet-exit))))
        (set-marker (yas/exit-marker snippet-exit) nil)
        (setf (yas/exit-marker snippet-exit) (cons exit (yas/exit-marker snippet-exit)))))))

(defun yas/points-to-markers (snippet)
  "Convert all cons (POINT . MARKER) in SNIPPET to markers. This
is done by setting MARKER to POINT with `set-marker'."
  (dolist (field (yas/snippet-fields snippet))
    (setf (yas/field-start field) (set-marker (cdr (yas/field-start field))
                                              (car (yas/field-start field))))
    (setf (yas/field-end field) (set-marker (cdr (yas/field-end field))
                                            (car (yas/field-end field))))
    (dolist (mirror (yas/field-mirrors field))
      (setf (yas/mirror-start mirror) (set-marker (cdr (yas/mirror-start mirror))
                                                  (car (yas/mirror-start mirror))))
      (setf (yas/mirror-end mirror) (set-marker (cdr (yas/mirror-end mirror))
                                                (car (yas/mirror-end mirror))))))
  (let ((snippet-exit (yas/snippet-exit snippet)))
    (when snippet-exit
      (setf (yas/exit-marker snippet-exit) (set-marker (cdr (yas/exit-marker snippet-exit))
                                                       (car (yas/exit-marker snippet-exit)))))))

(defun yas/commit-snippet (snippet &optional no-hooks)
  "Commit SNIPPET, but leave point as it is.  This renders the
snippet as ordinary text.

Return a buffer position where the point should be placed if
exiting the snippet.

NO-HOOKS means don't run the `yas/after-exit-snippet-hook' hooks."

  (let ((control-overlay (yas/snippet-control-overlay snippet))
        yas/snippet-beg
        yas/snippet-end)
    ;;
    ;; Save the end of the moribund snippet in case we need to revive it
    ;; its original expansion.
    ;;
    (when (and control-overlay
               (overlay-buffer control-overlay))
      (setq yas/snippet-beg (overlay-start control-overlay))
      (setq yas/snippet-end (overlay-end control-overlay))
      (delete-overlay control-overlay))

    (let ((inhibit-modification-hooks t))
      (when yas/active-field-overlay
        (delete-overlay yas/active-field-overlay))
      (when yas/field-protection-overlays
        (mapc #'delete-overlay yas/field-protection-overlays)))

    ;; stacked expansion: if the original expansion took place from a
    ;; field, make sure we advance it here at least to
    ;; `yas/snippet-end'...
    ;;
    (let ((previous-field (yas/snippet-previous-active-field snippet)))
      (when (and yas/snippet-end previous-field)
        (yas/advance-end-maybe previous-field yas/snippet-end)))

    ;; Convert all markers to points,
    ;;
    (yas/markers-to-points snippet)

    ;; Take care of snippet revival
    ;;
    (if yas/snippet-revival
        (push `(apply yas/snippet-revive ,yas/snippet-beg ,yas/snippet-end ,snippet)
              buffer-undo-list)
      ;; Dismember the snippet... this is useful if we get called
      ;; again from `yas/take-care-of-redo'....
      (setf (yas/snippet-fields snippet) nil))

    ;; XXX: `yas/after-exit-snippet-hook' should be run with
    ;; `yas/snippet-beg' and `yas/snippet-end' bound. That might not
    ;; be the case if the main overlay had somehow already
    ;; disappeared, which sometimes happens when the snippet's messed
    ;; up...
    ;;
    (unless no-hooks (run-hooks 'yas/after-exit-snippet-hook)))

  (message "[yas] snippet exited."))

(defun yas/check-commit-snippet ()
  "Checks if point exited the currently active field of the
snippet, if so cleans up the whole snippet up."
  (let* ((snippets (yas/snippets-at-point 'all-snippets))
         (snippets-left snippets))
    (dolist (snippet snippets)
      (let ((active-field (yas/snippet-active-field snippet)))
        (cond ((or (prog1 (yas/snippet-force-exit snippet)
                     (setf (yas/snippet-force-exit snippet) nil))
                   (not (and active-field (yas/field-contains-point-p active-field))))
               (setq snippets-left (delete snippet snippets-left))
               (yas/commit-snippet snippet snippets-left))
              ((and active-field
                    (or (not yas/active-field-overlay)
                        (not (overlay-buffer yas/active-field-overlay))))
               ;;
               ;; stacked expansion: this case is mainly for recent
               ;; snippet exits that place us back int the field of
               ;; another snippet
               ;;
               (save-excursion
                 (yas/move-to-field snippet active-field)
                 (yas/update-mirrors snippet)))
              (t
               nil))))
    (unless snippets-left
      (remove-hook 'post-command-hook 'yas/post-command-handler 'local)
      (remove-hook 'pre-command-hook 'yas/pre-command-handler 'local))))

(defun yas/field-contains-point-p (field &optional point)
  (let ((point (or point
                   (point))))
    (and (>= point (yas/field-start field))
         (<= point (yas/field-end field)))))

(defun yas/field-text-for-display (field)
  "Return the propertized display text for field FIELD.  "
  (buffer-substring (yas/field-start field) (yas/field-end field)))

(defun yas/undo-in-progress ()
  "True if some kind of undo is in progress"
  (or undo-in-progress
      (eq this-command 'undo)
      (eq this-command 'redo)))

(defun yas/make-control-overlay (snippet start end)
  "Creates the control overlay that surrounds the snippet and
holds the keymap."
  (let ((overlay (make-overlay start
                               end
                               nil
                               nil
                               t)))
    (overlay-put overlay 'keymap yas/keymap)
    (overlay-put overlay 'yas/snippet snippet)
    overlay))

(defun yas/skip-and-clear-or-delete-char (&optional field)
  "Clears unmodified field if at field start, skips to next tab.

Otherwise deletes a character normally by calling `delete-char'."
  (interactive)
  (let ((field (or field
                   (and yas/active-field-overlay
                        (overlay-buffer yas/active-field-overlay)
                        (overlay-get yas/active-field-overlay 'yas/field)))))
    (cond ((and field
                (not (yas/field-modified-p field))
                (eq (point) (marker-position (yas/field-start field))))
           (yas/skip-and-clear field)
           (yas/next-field 1))
          (t
           (call-interactively 'delete-char)))))

(defun yas/skip-and-clear (field)
  "Deletes the region of FIELD and sets it modified state to t"
  (setf (yas/field-modified-p field) t)
  (delete-region (yas/field-start field) (yas/field-end field)))

(defun yas/make-move-active-field-overlay (snippet field)
  "Place the active field overlay in SNIPPET's FIELD.

Move the overlay, or create it if it does not exit."
  (if (and yas/active-field-overlay
           (overlay-buffer yas/active-field-overlay))
      (move-overlay yas/active-field-overlay
                    (yas/field-start field)
                    (yas/field-end field))
    (setq yas/active-field-overlay
          (make-overlay (yas/field-start field)
                        (yas/field-end field)
                        nil nil t))
    (overlay-put yas/active-field-overlay 'priority 100)
    (overlay-put yas/active-field-overlay 'face 'yas/field-highlight-face)
    (overlay-put yas/active-field-overlay 'yas/snippet snippet)
    (overlay-put yas/active-field-overlay 'modification-hooks '(yas/on-field-overlay-modification))
    (overlay-put yas/active-field-overlay 'insert-in-front-hooks
                 '(yas/on-field-overlay-modification))
    (overlay-put yas/active-field-overlay 'insert-behind-hooks
                 '(yas/on-field-overlay-modification))))

(defun yas/on-field-overlay-modification (overlay after? beg end &optional length)
  "Clears the field and updates mirrors, conditionally.

Only clears the field if it hasn't been modified and it point it
at field start. This hook doesn't do anything if an undo is in
progress."
  (unless (yas/undo-in-progress)
    (let ((field (overlay-get yas/active-field-overlay 'yas/field)))
      (cond (after?
             (yas/advance-end-maybe field (overlay-end overlay))
;;; primary field transform: normal calls to expression
             (let ((saved-point (point)))
               (yas/field-update-display field (car (yas/snippets-at-point)))
               (goto-char saved-point))
             (yas/update-mirrors (car (yas/snippets-at-point))))
            (field
             (when (and (not after?)
                        (not (yas/field-modified-p field))
                        (eq (point) (if (markerp (yas/field-start field))
                                        (marker-position (yas/field-start field))
                                      (yas/field-start field))))
               (yas/skip-and-clear field))
             (setf (yas/field-modified-p field) t))))))

;;; Apropos protection overlays:
;;;
;;; These exist for nasty users who will try to delete parts of the
;;; snippet outside the active field. Actual protection happens in
;;; `yas/on-protection-overlay-modification'.
;;;
;;; Currently this signals an error which inhibits the command. For
;;; commands that move point (like `kill-line'), point is restored in
;;; the `yas/post-command-handler' using a global
;;; `yas/protection-violation' variable.
;;;
;;; Alternatively, I've experimented with an implementation that
;;; commits the snippet before actually calling `this-command'
;;; interactively, and then signals an eror, which is ignored. but
;;; blocks all other million modification hooks. This presented some
;;; problems with stacked expansion.
;;;

(defun yas/make-move-field-protection-overlays (snippet field)
  "Place protection overlays surrounding SNIPPET's FIELD.

Move the overlays, or create them if they do not exit."
  (let ((start (yas/field-start field))
        (end (yas/field-end field)))
    ;; First check if the (1+ end) is contained in the buffer,
    ;; otherwise we'll have to do a bit of cheating and silently
    ;; insert a newline. the `(1+ (buffer-size))' should prevent this
    ;; when using stacked expansion
    ;;
    (when (< (buffer-size) end)
      (save-excursion
        (let ((inhibit-modification-hooks t))
          (goto-char (point-max))
          (newline))))
    ;; go on to normal overlay creation/moving
    ;;
    (cond ((and yas/field-protection-overlays
                (every #'overlay-buffer yas/field-protection-overlays))
           (move-overlay (first yas/field-protection-overlays) (1- start) start)
           (move-overlay (second yas/field-protection-overlays) end (1+ end)))
          (t
           (setq yas/field-protection-overlays
                 (list (make-overlay (1- start) start nil t nil)
                       (make-overlay end (1+ end) nil t nil)))
           (dolist (ov yas/field-protection-overlays)
             (overlay-put ov 'face 'yas/field-debug-face)
             (overlay-put ov 'yas/snippet snippet)
             ;; (overlay-put ov 'evaporate t)
             (overlay-put ov 'modification-hooks '(yas/on-protection-overlay-modification)))))))

(defvar yas/protection-violation nil
  "When non-nil, signals attempts to erronesly exit or modify the snippet.

Functions in the `post-command-hook', for example
`yas/post-command-handler' can check it and reset its value to
nil. The variables value is the point where the violation
originated")

(defun yas/on-protection-overlay-modification (overlay after? beg end &optional length)
  "Signals a snippet violation, then issues error.

The error should be ignored in `debug-ignored-errors'"
  (cond ((not (or after?
                  (yas/undo-in-progress)))
         (setq yas/protection-violation (point))
         (error "Exit the snippet first!"))))

(add-to-list 'debug-ignored-errors "^Exit the snippet first!$")


;;; Apropos stacked expansion:
;;;
;;; the parent snippet does not run its fields modification hooks
;;; (`yas/on-field-overlay-modification' and
;;; `yas/on-protection-overlay-modification') while the child snippet
;;; is active. This means, among other things, that the mirrors of the
;;; parent snippet are not updated, this only happening when one exits
;;; the child snippet.
;;;
;;; Unfortunately, this also puts some ugly (and not fully-tested)
;;; bits of code in `yas/expand-snippet' and
;;; `yas/commit-snippet'. I've tried to mark them with "stacked
;;; expansion:".
;;;
;;; This was thought to be safer in in an undo/redo perpective, but
;;; maybe the correct implementation is to make the globals
;;; `yas/active-field-overlay' and `yas/field-protection-overlays' be
;;; snippet-local and be active even while the child snippet is
;;; running. This would mean a lot of overlay modification hooks
;;; running, but if managed correctly (including overlay priorities)
;;; they should account for all situations...
;;;

(defun yas/expand-snippet (template &optional start end expand-env)
  "Expand snippet at current point. Text between START and END
will be deleted before inserting template."
  (run-hooks 'yas/before-expand-snippet-hook)

  ;; If a region is active, set `yas/selected-text'
  (setq yas/selected-text
        (when mark-active
          (prog1 (buffer-substring-no-properties (region-beginning)
                                                 (region-end))
            (unless start (setq start (region-beginning))
                    (unless end (setq end (region-end)))))))

  (when start
    (goto-char start))

  ;; stacked expansion: shoosh the overlay modification hooks
  ;;
  (let ((to-delete (and start end (buffer-substring-no-properties start end)))
        (start (or start (point)))
        (end (or end (point)))
        (inhibit-modification-hooks t)
        (column (current-column))
        snippet)

    ;; Delete the region to delete, this *does* get undo-recorded.
    ;;
    (when (and to-delete
               (> end start))
      (delete-region start end)
      (setq yas/deleted-text to-delete))

    ;; Narrow the region down to the template, shoosh the
    ;; `buffer-undo-list', and create the snippet, the new snippet
    ;; updates its mirrors once, so we are left with some plain text.
    ;; The undo action for deleting this plain text will get recorded
    ;; at the end of this function.
    (save-restriction
      (narrow-to-region start start)
      (let ((buffer-undo-list t))
        ;; snippet creation might evaluate users elisp, which
        ;; might generate errors, so we have to be ready to catch
        ;; them mostly to make the undo information
        ;;
        (setq yas/start-column (save-restriction (widen) (current-column)))
        (insert template)

        (setq snippet
              (if expand-env
                  (let ((read-vars (condition-case err
                                       (read expand-env)
                                     (error nil))))
                    (eval `(let ,read-vars
                             (yas/snippet-create (point-min) (point-max)))))
                (yas/snippet-create (point-min) (point-max))))))

    ;; stacked-expansion: This checks for stacked expansion, save the
    ;; `yas/previous-active-field' and advance its boudary.
    ;;
    (let ((existing-field (and yas/active-field-overlay
                               (overlay-buffer yas/active-field-overlay)
                               (overlay-get yas/active-field-overlay 'yas/field))))
      (when existing-field
        (setf (yas/snippet-previous-active-field snippet) existing-field)
        (yas/advance-end-maybe existing-field (overlay-end yas/active-field-overlay))))

    ;; Exit the snippet immediately if no fields
    ;;
    (unless (yas/snippet-fields snippet)
      (yas/exit-snippet snippet))

    ;; Push two undo actions: the deletion of the inserted contents of
    ;; the new snippet (without the "key") followed by an apply of
    ;; `yas/take-care-of-redo' on the newly inserted snippet boundaries
    ;;
    (let ((start (overlay-start (yas/snippet-control-overlay snippet)))
          (end (overlay-end (yas/snippet-control-overlay snippet))))
      (push (cons start end) buffer-undo-list)
      (push `(apply yas/take-care-of-redo ,start ,end ,snippet)
            buffer-undo-list))
    ;; Now, move to the first field
    ;;
    (let ((first-field (car (yas/snippet-fields snippet))))
      (when first-field
        (yas/move-to-field snippet first-field))))
  (message "[yas] snippet expanded."))

(defun yas/take-care-of-redo (beg end snippet)
  "Commits SNIPPET, which in turn pushes an undo action for
reviving it.

Meant to exit in the `buffer-undo-list'."
  ;; slightly optimize: this action is only needed for snippets with
  ;; at least one field
  (when (yas/snippet-fields snippet)
    (yas/commit-snippet snippet 'no-hooks)))

(defun yas/snippet-revive (beg end snippet)
  "Revives the SNIPPET and creates a control overlay from BEG to
END.

BEG and END are, we hope, the original snippets boudaries. All
the markers/points exiting existing inside SNIPPET should point
to their correct locations *at the time the snippet is revived*.

After revival, push the `yas/take-care-of-redo' in the
`buffer-undo-list'"
  ;; Reconvert all the points to markers
  ;;
  (yas/points-to-markers snippet)
  ;; When at least one editable field existed in the zombie snippet,
  ;; try to revive the whole thing...
  ;;
  (let ((target-field (or (yas/snippet-active-field snippet)
                          (car (yas/snippet-fields snippet)))))
    (when target-field
      (setf (yas/snippet-control-overlay snippet) (yas/make-control-overlay snippet beg end))
      (overlay-put (yas/snippet-control-overlay snippet) 'yas/snippet snippet)

      (yas/move-to-field snippet target-field)

      (add-hook 'post-command-hook 'yas/post-command-handler nil t)
      (add-hook 'pre-command-hook 'yas/pre-command-handler t t)

      (push `(apply yas/take-care-of-redo ,beg ,end ,snippet)
            buffer-undo-list))))

(defun yas/snippet-create (begin end)
  "Creates a snippet from an template inserted between BEGIN and END.

Returns the newly created snippet."
  (let ((snippet (yas/make-snippet)))
    (goto-char begin)
    (yas/snippet-parse-create snippet)

    ;; Sort and link each field
    (yas/snippet-sort-fields snippet)

    ;; Create keymap overlay for snippet
    (setf (yas/snippet-control-overlay snippet)
          (yas/make-control-overlay snippet (point-min) (point-max)))

    ;; Move to end
    (goto-char (point-max))

    ;; Setup hooks
    (add-hook 'post-command-hook 'yas/post-command-handler nil t)
    (add-hook 'pre-command-hook 'yas/pre-command-handler t t)

    snippet))


;;; Apropos adjacencies: Once the $-constructs bits like "$n" and
;;; "${:n" are deleted in the recently expanded snippet, we might
;;; actually have many fields, mirrors (and the snippet exit) in the
;;; very same position in the buffer. Therefore we need to single-link
;;; the fields-or-mirrors-or-exit, which I have called "fom",
;;; according to their original positions in the buffer.
;;;
;;; Then we have operation `yas/advance-end-maybe' and
;;; `yas/advance-start-maybe', which conditionally push the starts and
;;; ends of these foms down the chain.
;;;
;;; This allows for like the printf with the magic ",":
;;;
;;;   printf ("${1:%s}\\n"${1:$(if (string-match "%" text) "," "\);")}  \
;;;   $2${1:$(if (string-match "%" text) "\);" "")}$0
;;;

(defun yas/fom-start (fom)
  (cond ((yas/field-p fom)
         (yas/field-start fom))
        ((yas/mirror-p fom)
         (yas/mirror-start fom))
        (t
         (yas/exit-marker fom))))

(defun yas/fom-end (fom)
  (cond ((yas/field-p fom)
         (yas/field-end fom))
        ((yas/mirror-p fom)
         (yas/mirror-end fom))
        (t
         (yas/exit-marker fom))))

(defun yas/fom-next (fom)
  (cond ((yas/field-p fom)
         (yas/field-next fom))
        ((yas/mirror-p fom)
         (yas/mirror-next fom))
        (t
         (yas/exit-next fom))))

(defun yas/calculate-adjacencies (snippet)
  "Calculate adjacencies for fields or mirrors of SNIPPET.

This is according to their relative positions in the buffer, and
has to be called before the $-constructs are deleted."
  (flet ((yas/fom-set-next-fom (fom nextfom)
                               (cond ((yas/field-p fom)
                                      (setf (yas/field-next fom) nextfom))
                                     ((yas/mirror-p fom)
                                      (setf (yas/mirror-next fom) nextfom))
                                     (t
                                      (setf (yas/exit-next fom) nextfom))))
         (yas/compare-fom-begs (fom1 fom2)
                               (> (yas/fom-start fom2) (yas/fom-start fom1)))
         (yas/link-foms (fom1 fom2)
                        (yas/fom-set-next-fom fom1 fom2)))
    ;; make some yas/field, yas/mirror and yas/exit soup
    (let ((soup))
      (when (yas/snippet-exit snippet)
        (push (yas/snippet-exit snippet) soup))
      (dolist (field (yas/snippet-fields snippet))
        (push field soup)
        (dolist (mirror (yas/field-mirrors field))
          (push mirror soup)))
      (setq soup
            (sort soup
                  #'yas/compare-fom-begs))
      (when soup
        (reduce #'yas/link-foms soup)))))

(defun yas/advance-end-maybe (fom newend)
  "Maybe advance FOM's end to NEWEND if it needs it.

If it does, also:

* call `yas/advance-start-maybe' on FOM's next fom.

* in case FOM is field call `yas/advance-end-maybe' on its parent
  field"
  (when (and fom (< (yas/fom-end fom) newend))
    (set-marker (yas/fom-end fom) newend)
    (yas/advance-start-maybe (yas/fom-next fom) newend)
    (if (and (yas/field-p fom)
             (yas/field-parent-field fom))
        (yas/advance-end-maybe (yas/field-parent-field fom) newend))))

(defun yas/advance-start-maybe (fom newstart)
  "Maybe advance FOM's start to NEWSTART if it needs it.

If it does, also call `yas/advance-end-maybe' on FOM."
  (when (and fom (< (yas/fom-start fom) newstart))
    (set-marker (yas/fom-start fom) newstart)
    (yas/advance-end-maybe fom newstart)))

(defvar yas/dollar-regions nil
  "When expanding the snippet the \"parse-create\" functions add
  cons cells to this var")

(defun yas/snippet-parse-create (snippet)
  "Parse a recently inserted snippet template, creating all
necessary fields, mirrors and exit points.

Meant to be called in a narrowed buffer, does various passes"
  (let ((parse-start (point)))
    ;; Reset the yas/dollar-regions
    ;;
    (setq yas/dollar-regions nil)
    ;; protect escaped quote, backquotes and backslashes
    ;;
    (yas/protect-escapes nil '(?\\ ?` ?'))
    ;; replace all backquoted expressions
    ;;
    (goto-char parse-start)
    (yas/replace-backquotes)
    ;; protect escapes again since previous steps might have generated
    ;; more characters needing escaping
    ;;
    (goto-char parse-start)
    (yas/protect-escapes)
    ;; parse fields with {}
    ;;
    (goto-char parse-start)
    (yas/field-parse-create snippet)
    ;; parse simple mirrors and fields
    ;;
    (goto-char parse-start)
    (yas/simple-mirror-parse-create snippet)
    ;; parse mirror transforms
    ;;
    (goto-char parse-start)
    (yas/transform-mirror-parse-create snippet)
    ;; calculate adjacencies of fields and mirrors
    ;;
    (yas/calculate-adjacencies snippet)
    ;; Delete $-constructs
    ;;
    (yas/delete-regions yas/dollar-regions)
    ;; restore escapes
    ;;
    (goto-char parse-start)
    (yas/restore-escapes)
    ;; update mirrors for the first time
    ;;
    (yas/update-mirrors snippet)
    ;; indent the best we can
    ;;
    (goto-char parse-start)
    (yas/indent snippet)))

(defun yas/indent-according-to-mode (snippet-markers)
  "Indent current line according to mode, preserving
SNIPPET-MARKERS."
  ;; XXX: Here seems to be the indent problem:
  ;;
  ;; `indent-according-to-mode' uses whatever
  ;; `indent-line-function' is available. Some
  ;; implementations of these functions delete text
  ;; before they insert. If there happens to be a marker
  ;; just after the text being deleted, the insertion
  ;; actually happens  after the marker, which misplaces
  ;; it.
  ;;
  ;; This would also happen if we had used overlays with
  ;; the `front-advance' property set to nil.
  ;;
  ;; This is why I have these `trouble-markers', they are the ones at
  ;; they are the ones at the first non-whitespace char at the line
  ;; (i.e. at `yas/real-line-beginning'. After indentation takes place
  ;; we should be at the correct to restore them to. All other
  ;; non-trouble-markers have been *pushed* and don't need special
  ;; attention.
  ;;
  (goto-char (yas/real-line-beginning))
  (let ((trouble-markers (remove-if-not #'(lambda (marker)
                                            (= marker (point)))
                                        snippet-markers)))
    (save-restriction
      (widen)
      (condition-case err
          (indent-according-to-mode)
        (error (message "[yas] warning: yas/indent-according-to-mode habing problems running %s" indent-line-function)
               nil)))
    (mapc #'(lambda (marker)
              (set-marker marker (point)))
          trouble-markers)))

(defun yas/indent (snippet)
  (let ((snippet-markers (yas/collect-snippet-markers snippet)))
    ;; Look for those $>
    (save-excursion
      (while (re-search-forward "$>" nil t)
        (delete-region (match-beginning 0) (match-end 0))
        (when (not (eq yas/indent-line 'auto))
          (yas/indent-according-to-mode snippet-markers))))
    ;; Now do stuff for 'fixed and 'auto
    (save-excursion
      (cond ((eq yas/indent-line 'fixed)
             (while (and (zerop (forward-line))
                         (zerop (current-column)))
               (indent-to-column column)))
            ((eq yas/indent-line 'auto)
             (let ((end (set-marker (make-marker) (point-max)))
                   (indent-first-line-p yas/also-auto-indent-first-line))
               (while (and (zerop (if indent-first-line-p
                                      (prog1
                                          (forward-line 0)
                                        (setq indent-first-line-p nil))
                                    (forward-line 1)))
                           (not (eobp))
                           (<= (point) end))
                 (yas/indent-according-to-mode snippet-markers))))
            (t
             nil)))))

(defun yas/collect-snippet-markers (snippet)
  "Make a list of all the markers used by SNIPPET."
  (let (markers)
    (dolist (field (yas/snippet-fields snippet))
      (push (yas/field-start field) markers)
      (push (yas/field-end field) markers)
      (dolist (mirror (yas/field-mirrors field))
        (push (yas/mirror-start mirror) markers)
        (push (yas/mirror-end mirror) markers)))
    (let ((snippet-exit (yas/snippet-exit snippet)))
      (when (and snippet-exit
                 (marker-buffer (yas/exit-marker snippet-exit)))
        (push (yas/exit-marker snippet-exit) markers)))
    markers))

(defun yas/real-line-beginning ()
  (let ((c (char-after (line-beginning-position)))
        (n (line-beginning-position)))
    (while (or (eql c ?\ )
               (eql c ?\t))
      (incf n)
      (setq c (char-after n)))
    n))

(defun yas/escape-string (escaped)
  (concat "YASESCAPE" (format "%d" escaped) "PROTECTGUARD"))

(defun yas/protect-escapes (&optional text escaped)
  "Protect all escaped characters with their numeric ASCII value.

With optional string TEXT do it in string instead of buffer."
  (let ((changed-text text)
        (text-provided-p text))
    (mapc #'(lambda (escaped)
              (setq changed-text
                    (yas/replace-all (concat "\\" (char-to-string escaped))
                                     (yas/escape-string escaped)
                                     (when text-provided-p changed-text))))
          (or escaped yas/escaped-characters))
    changed-text))

(defun yas/restore-escapes (&optional text escaped)
  "Restore all escaped characters from their numeric ASCII value.

With optional string TEXT do it in string instead of the buffer."
  (let ((changed-text text)
        (text-provided-p text))
    (mapc #'(lambda (escaped)
              (setq changed-text
                    (yas/replace-all (yas/escape-string escaped)
                                     (char-to-string escaped)
                                     (when text-provided-p changed-text))))
          (or escaped yas/escaped-characters))
    changed-text))

(defun yas/replace-backquotes ()
  "Replace all the \"`(lisp-expression)`\"-style expression
  with their evaluated value"
  (while (re-search-forward yas/backquote-lisp-expression-regexp nil t)
    (let ((transformed (yas/read-and-eval-string (yas/restore-escapes (match-string 1)))))
      (goto-char (match-end 0))
      (when transformed (insert transformed))
      (delete-region (match-beginning 0) (match-end 0)))))

(defun yas/scan-sexps (from count)
  (condition-case err
      (with-syntax-table (standard-syntax-table)
        (scan-sexps from count))
    (error
     nil)))

(defun yas/make-marker (pos)
  "Create a marker at POS with `nil' `marker-insertion-type'"
  (let ((marker (set-marker (make-marker) pos)))
    (set-marker-insertion-type marker nil)
    marker))

(defun yas/field-parse-create (snippet &optional parent-field)
  "Parse most field expressions, except for the simple one \"$n\".

The following count as a field:

* \"${n: text}\", for a numbered field with default text, as long as N is not 0;

* \"${n: text$(expression)}, the same with a lisp expression;
  this is caught with the curiously named `yas/multi-dollar-lisp-expression-regexp'

* the same as above but unnumbered, (no N:) and number is calculated automatically.

When multiple expressions are found, only the last one counts."
  ;;
  (save-excursion
    (while (re-search-forward yas/field-regexp nil t)
      (let* ((real-match-end-0 (yas/scan-sexps (1+ (match-beginning 0)) 1))
             (number (and (match-string-no-properties 1)
                          (string-to-number (match-string-no-properties 1))))
             (brand-new-field (and real-match-end-0
                                   ;; break if on "$(" immediately
                                   ;; after the ":", this will be
                                   ;; caught as a mirror with
                                   ;; transform later.
                                   (not (save-match-data
                                          (eq (string-match "$[ \t\n]*("
                                                            (match-string-no-properties 2)) 0)))
                                   (not (and number (zerop number)))
                                   (yas/make-field number
                                                   (yas/make-marker (match-beginning 2))
                                                   (yas/make-marker (1- real-match-end-0))
                                                   parent-field))))
        (when brand-new-field
          (goto-char real-match-end-0)
          (push (cons (1- real-match-end-0) real-match-end-0)
                yas/dollar-regions)
          (push (cons (match-beginning 0) (match-beginning 2))
                yas/dollar-regions)
          (push brand-new-field (yas/snippet-fields snippet))
          (save-excursion
            (save-restriction
              (narrow-to-region (yas/field-start brand-new-field) (yas/field-end brand-new-field))
              (goto-char (point-min))
              (yas/field-parse-create snippet brand-new-field)))))))
  ;; if we entered from a parent field, now search for the
  ;; `yas/multi-dollar-lisp-expression-regexp'. THis is used for
  ;; primary field transformations
  ;; 
  (when parent-field
    (save-excursion
      (while (re-search-forward yas/multi-dollar-lisp-expression-regexp nil t)
        (let* ((real-match-end-1 (yas/scan-sexps (match-beginning 1) 1)))
          ;; commit the primary field transformation if we don't find
          ;; it in yas/dollar-regions (a subnested field) might have
          ;; already caught it.
          (when (and real-match-end-1
                     (not (member (cons (match-beginning 0)
                                        real-match-end-1)
                                  yas/dollar-regions)))
            (let ((lisp-expression-string (buffer-substring-no-properties (match-beginning 1)
                                                                          real-match-end-1)))
              (setf (yas/field-transform parent-field) (yas/restore-escapes lisp-expression-string)))
            (push (cons (match-beginning 0) real-match-end-1)
                  yas/dollar-regions)))))))

(defun yas/transform-mirror-parse-create (snippet)
  "Parse the \"${n:$(lisp-expression)}\" mirror transformations."
  (while (re-search-forward yas/transform-mirror-regexp nil t)
    (let* ((real-match-end-0 (yas/scan-sexps (1+ (match-beginning 0)) 1))
           (number (string-to-number (match-string-no-properties 1)))
           (field (and number
                       (not (zerop number))
                       (yas/snippet-find-field snippet number))))
      (when (and real-match-end-0
                 field)
        (push (yas/make-mirror (yas/make-marker (match-beginning 0))
                               (yas/make-marker (match-beginning 0))
                               (yas/restore-escapes
                                (buffer-substring-no-properties (match-beginning 2)
                                                                (1- real-match-end-0))))
              (yas/field-mirrors field))
        (push (cons (match-beginning 0) real-match-end-0) yas/dollar-regions)))))

(defun yas/simple-mirror-parse-create (snippet)
  "Parse the simple \"$n\" mirrors and the exit-marker."
  (while (re-search-forward yas/simple-mirror-regexp nil t)
    (let ((number (string-to-number (match-string-no-properties 1))))
      (cond ((zerop number)

             (setf (yas/snippet-exit snippet)
                   (yas/make-exit (yas/make-marker (match-end 0))))
             (save-excursion
               (goto-char (match-beginning 0))
               (when yas/wrap-around-region
                 (cond (yas/selected-text
                        (insert yas/selected-text))
                       ((and (eq yas/wrap-around-region 'cua)
                             cua-mode
                             (get-register ?0))
                        (insert (prog1 (get-register ?0)
                                  (set-register ?0 nil))))))
               (push (cons (point) (yas/exit-marker (yas/snippet-exit snippet)))
                     yas/dollar-regions)))
            (t
             (let ((field (yas/snippet-find-field snippet number)))
               (if field
                   (push (yas/make-mirror (yas/make-marker (match-beginning 0))
                                          (yas/make-marker (match-beginning 0))
                                          nil)
                         (yas/field-mirrors field))
                 (push (yas/make-field number
                                       (yas/make-marker (match-beginning 0))
                                       (yas/make-marker (match-beginning 0))
                                       nil)
                       (yas/snippet-fields snippet))))
             (push (cons (match-beginning 0) (match-end 0))
                   yas/dollar-regions))))))

(defun yas/delete-regions (regions)
  "Sort disjuct REGIONS by start point, then delete from the back."
  (mapc #'(lambda (reg)
            (delete-region (car reg) (cdr reg)))
        (sort regions
              #'(lambda (r1 r2)
                  (>= (car r1) (car r2))))))

(defun yas/update-mirrors (snippet)
  "Updates all the mirrors of SNIPPET."
  (save-excursion
    (dolist (field (yas/snippet-fields snippet))
      (dolist (mirror (yas/field-mirrors field))
        ;; stacked expansion: I added an `inhibit-modification-hooks'
        ;; here, for safety, may need to remove if we the mechanism is
        ;; altered.
        ;;
        (let ((inhibit-modification-hooks t))
          (yas/mirror-update-display mirror field)
          ;; `yas/place-overlays' is needed if the active field and
          ;; protected overlays have been changed because of insertions
          ;; in `yas/mirror-update-display'
          ;;
          (when (eq field (yas/snippet-active-field snippet))
            (yas/place-overlays snippet field)))))))

(defun yas/mirror-update-display (mirror field)
  "Update MIRROR according to FIELD (and mirror transform)."
  (let ((reflection (or (yas/apply-transform mirror field)
                        (yas/field-text-for-display field))))
    (when (and reflection
               (not (string= reflection (buffer-substring-no-properties (yas/mirror-start mirror)
                                                                        (yas/mirror-end mirror)))))
      (goto-char (yas/mirror-start mirror))
      (insert reflection)
      (if (> (yas/mirror-end mirror) (point))
          (delete-region (point) (yas/mirror-end mirror))
        (set-marker (yas/mirror-end mirror) (point))
        (yas/advance-start-maybe (yas/mirror-next mirror) (point))))))

(defun yas/field-update-display (field snippet)
  "Much like `yas/mirror-update-display', but for fields"
  (when (yas/field-transform field)
    (let ((inhibit-modification-hooks t)
          (transformed (yas/apply-transform field field))
          (point (point)))
      (when (and transformed
                 (not (string= transformed (buffer-substring-no-properties (yas/field-start field)
                                                                           (yas/field-end field)))))
        (setf (yas/field-modified-p field) t)
        (goto-char (yas/field-start field))
        (insert transformed)
        (if (> (yas/field-end field) (point))
            (delete-region (point) (yas/field-end field))
          (set-marker (yas/field-end field) (point))
          (yas/advance-start-maybe (yas/field-next field) (point)))
        t))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Pre- and post-command hooks
;;
(defun yas/pre-command-handler () )

(defun yas/post-command-handler ()
  "Handles various yasnippet conditions after each command."
  (cond (yas/protection-violation
         (goto-char yas/protection-violation)
         (setq yas/protection-violation nil))
        ((eq 'undo this-command)
         ;;
         ;; After undo revival the correct field is sometimes not
         ;; restored correctly, this condition handles that
         ;;
         (let* ((snippet (car (yas/snippets-at-point)))
                (target-field (and snippet
                                   (find-if-not #'(lambda (field)
                                                    (yas/field-probably-deleted-p snippet field))
                                                (remove nil
                                                        (cons (yas/snippet-active-field snippet)
                                                              (yas/snippet-fields snippet)))))))
           (when target-field
             (yas/move-to-field snippet target-field))))
        ((not (yas/undo-in-progress))
         ;; When not in an undo, check if we must commit the snippet (use exited it).
         (yas/check-commit-snippet))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Debug functions.  Use (or change) at will whenever needed.
;;
;; some useful debug code for looking up snippet tables
;;
;; (insert (pp
;; (let ((shit))
;;   (maphash #'(lambda (k v)
;;             (push k shit))
;;         (yas/snippet-table-hash (gethash 'ruby-mode yas/snippet-tables)))
;;   shit)))
;;

(defun yas/debug-tables ()
  (interactive)
  (with-output-to-temp-buffer "*YASnippet tables*"
    (dolist (symbol (remove nil (append (list major-mode)
                                        (if (listp yas/mode-symbol)
                                            yas/mode-symbol
                                          (list yas/mode-symbol)))))
      (princ (format "Snippet table hash keys for %s:\n\n" symbol))
      (let ((keys))
        (maphash #'(lambda (k v)
                     (push k keys))
                 (yas/snippet-table-hash (gethash symbol yas/snippet-tables)))
        (princ keys))

      (princ (format "Keymap for  %s:\n\n" symbol))
      (princ (gethash symbol yas/menu-table)))))

(defun yas/debug-snippet-vars ()
  "Debug snippets, fields, mirrors and the `buffer-undo-list'."
  (interactive)
  (with-output-to-temp-buffer "*YASnippet trace*"
    (princ "Interesting YASnippet vars: \n\n")

    (princ (format "\nPost command hook: %s\n" post-command-hook))
    (princ (format "\nPre  command hook: %s\n" pre-command-hook))

    (princ (format "%s live snippets in total\n" (length (yas/snippets-at-point (quote all-snippets)))))
    (princ (format "%s overlays in buffer:\n\n" (length (overlays-in (point-min) (point-max)))))
    (princ (format "%s live snippets at point:\n\n" (length (yas/snippets-at-point))))


    (dolist (snippet (yas/snippets-at-point))
      (princ (format "\tsid: %d control overlay from %d to %d\n"
                     (yas/snippet-id snippet)
                     (overlay-start (yas/snippet-control-overlay snippet))
                     (overlay-end (yas/snippet-control-overlay snippet))))
      (princ (format "\tactive field: %d from %s to %s covering \"%s\"\n"
                     (yas/field-number (yas/snippet-active-field snippet))
                     (marker-position (yas/field-start (yas/snippet-active-field snippet)))
                     (marker-position (yas/field-end (yas/snippet-active-field snippet)))
                     (buffer-substring-no-properties (yas/field-start (yas/snippet-active-field snippet)) (yas/field-end (yas/snippet-active-field snippet)))))
      (when (yas/snippet-exit snippet)
        (princ (format "\tsnippet-exit: at %s next: %s\n"
                       (yas/exit-marker (yas/snippet-exit snippet))
                       (yas/exit-next (yas/snippet-exit snippet)))))
      (dolist (field (yas/snippet-fields snippet))
        (princ (format "\tfield: %d from %s to %s covering \"%s\" next: %s\n"
                       (yas/field-number field)
                       (marker-position (yas/field-start field))
                       (marker-position (yas/field-end field))
                       (buffer-substring-no-properties (yas/field-start field) (yas/field-end field))
                       (yas/debug-format-fom-concise (yas/field-next field))))
        (dolist (mirror (yas/field-mirrors field))
          (princ (format "\t\tmirror: from %s to %s covering \"%s\" next: %s\n"
                         (marker-position (yas/mirror-start mirror))
                         (marker-position (yas/mirror-end mirror))
                         (buffer-substring-no-properties (yas/mirror-start mirror) (yas/mirror-end mirror))
                         (yas/debug-format-fom-concise (yas/mirror-next mirror)))))))

    (princ (format "\nUndo is %s and point-max is %s.\n"
                   (if (eq buffer-undo-list t)
                       "DISABLED"
                     "ENABLED")
                   (point-max)))
    (unless (eq buffer-undo-list t)
      (princ (format "Undpolist has %s elements. First 10 elements follow:\n" (length buffer-undo-list)))
      (let ((first-ten (subseq buffer-undo-list 0 19)))
        (dolist (undo-elem first-ten)
          (princ (format "%2s:  %s\n" (position undo-elem first-ten) (truncate-string-to-width (format "%s" undo-elem) 70))))))))

(defun yas/debug-format-fom-concise (fom)
  (when fom
    (cond ((yas/field-p fom)
           (format "field %d from %d to %d"
                   (yas/field-number fom)
                   (marker-position (yas/field-start fom))
                   (marker-position (yas/field-end fom))))
          ((yas/mirror-p fom)
           (format "mirror from %d to %d"
                   (marker-position (yas/mirror-start fom))
                   (marker-position (yas/mirror-end fom))))
          (t
           (format "snippet exit at %d"
                   (marker-position (yas/fom-start fom)))))))


(defun yas/exterminate-package ()
  (interactive)
  (yas/global-mode -1)
  (yas/minor-mode -1)
  (yas/kill-snippet-keybindings)
  (mapatoms #'(lambda (atom)
                (when (string-match "yas/" (symbol-name atom))
                  (unintern atom)))))

(defun yas/debug-test (&optional quiet)
  (interactive "P")
  (yas/load-directory (or (and (listp yas/root-directory)
                               (first yas/root-directory))
                          yas/root-directory
                          "~/Source/yasnippet/snippets/"))
  (set-buffer (switch-to-buffer "*YAS TEST*"))
  (mapc #'yas/commit-snippet (yas/snippets-at-point 'all-snippets))
  (erase-buffer)
  (setq buffer-undo-list nil)
  (setq undo-in-progress nil)
  (snippet-mode)
  (yas/minor-mode 1)
  (let ((abbrev))
    (setq abbrev "$f")
    (insert abbrev))
  (unless quiet
    (add-hook 'post-command-hook 'yas/debug-snippet-vars 't 'local)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; `locate-dominating-file' is added for compatibility in emacs < 23
(unless (or (eq emacs-major-version 23)
            (fboundp 'locate-dominating-file))
  (defvar locate-dominating-stop-dir-regexp
    "\\`\\(?:[\\/][\\/][^\\/]+[\\/]\\|/\\(?:net\\|afs\\|\\.\\.\\.\\)/\\)\\'"
    "Regexp of directory names which stop the search in `locate-dominating-file'.
Any directory whose name matches this regexp will be treated like
a kind of root directory by `locate-dominating-file' which will stop its search
when it bumps into it.
The default regexp prevents fruitless and time-consuming attempts to find
special files in directories in which filenames are interpreted as hostnames,
or mount points potentially requiring authentication as a different user.")

  (defun locate-dominating-file (file name)
    "Look up the directory hierarchy from FILE for a file named NAME.
Stop at the first parent directory containing a file NAME,
and return the directory.  Return nil if not found."
    ;; We used to use the above locate-dominating-files code, but the
    ;; directory-files call is very costly, so we're much better off doing
    ;; multiple calls using the code in here.
    ;;
    ;; Represent /home/luser/foo as ~/foo so that we don't try to look for
    ;; `name' in /home or in /.
    (setq file (abbreviate-file-name file))
    (let ((root nil)
          (prev-file file)
          ;; `user' is not initialized outside the loop because
          ;; `file' may not exist, so we may have to walk up part of the
          ;; hierarchy before we find the "initial UID".
          (user nil)
          try)
      (while (not (or root
                      (null file)
                      ;; FIXME: Disabled this heuristic because it is sometimes
                      ;; inappropriate.
                      ;; As a heuristic, we stop looking up the hierarchy of
                      ;; directories as soon as we find a directory belonging
                      ;; to another user.  This should save us from looking in
                      ;; things like /net and /afs.  This assumes that all the
                      ;; files inside a project belong to the same user.
                      ;; (let ((prev-user user))
                      ;;   (setq user (nth 2 (file-attributes file)))
                      ;;   (and prev-user (not (equal user prev-user))))
                      (string-match locate-dominating-stop-dir-regexp file)))
        (setq try (file-exists-p (expand-file-name name file)))
        (cond (try (setq root file))
              ((equal file (setq prev-file file
                                 file (file-name-directory
                                       (directory-file-name file))))
               (setq file nil))))
      root)))

(provide 'yasnippet)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Monkey patching for other functions that's causing
;; problems to yasnippet. For details on why I patch
;; those functions, refer to
;;   http://code.google.com/p/yasnippet/wiki/MonkeyPatching
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defadvice c-neutralize-syntax-in-CPP
  (around yas-mp/c-neutralize-syntax-in-CPP activate)
  "Adviced `c-neutralize-syntax-in-CPP' to properly
handle the end-of-buffer error fired in it by calling
`forward-char' at the end of buffer."
  (condition-case err
      ad-do-it
    (error (message (error-message-string err)))))

;; disable c-electric-* serial command in YAS fields
(add-hook 'c-mode-common-hook
          '(lambda ()
             (dolist (k '(":" ">" ";" "<" "{" "}"))
               (define-key (symbol-value (make-local-variable 'yas/keymap))
                 k 'self-insert-command))))


;;; yasnippet.el ends here

;;; dropdown-list.el --- Drop-down menu interface
;;
;; Filename: dropdown-list.el
;; Description: Drop-down menu interface
;; Author: Jaeyoun Chung [jay.chung@gmail.com]
;; Maintainer:
;; Copyright (C) 2008 Jaeyoun Chung
;; Created: Sun Mar 16 11:20:45 2008 (Pacific Daylight Time)
;; Version:
;; Last-Updated: Sun Mar 16 12:19:49 2008 (Pacific Daylight Time)
;;           By: dradams
;;     Update #: 43
;; URL: http://www.emacswiki.org/cgi-bin/wiki/dropdown-list.el
;; Keywords: convenience menu
;; Compatibility: GNU Emacs 21.x, GNU Emacs 22.x
;;
;; Features that might be required by this library:
;;
;;   `cl'.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;;  According to Jaeyoun Chung, "overlay code stolen from company-mode.el."
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Change log:
;;
;; 2008/03/16 dadams
;;     Clean-up - e.g. use char-to-string for control chars removed by email posting.
;;     Moved example usage code (define-key*, command-selector) inside the library.
;;     Require cl.el at byte-compile time.
;;     Added GPL statement.
;; 2008/01/06 Jaeyoun Chung
;;     Posted to gnu-emacs-sources@gnu.org at 9:10 p.m.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
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

(eval-when-compile (require 'cl)) ;; decf, fourth, incf, loop, mapcar*

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defface dropdown-list-face
  '((t :inherit default :background "lightyellow" :foreground "black"))
  "*Bla." :group 'dropdown-list)

(defface dropdown-list-selection-face
  '((t :inherit dropdown-list-face :background "purple"))
  "*Bla." :group 'dropdown-list)

(defvar dropdown-list-overlays nil)

(defun dropdown-list-hide ()
  (while dropdown-list-overlays
    (delete-overlay (pop dropdown-list-overlays))))

(defun dropdown-list-put-overlay (beg end &optional prop value prop2 value2)
  (let ((ov (make-overlay beg end)))
    (overlay-put ov 'window t)
    (when prop
      (overlay-put ov prop value)
      (when prop2 (overlay-put ov prop2 value2)))
    ov))

(defun dropdown-list-line (start replacement &optional no-insert)
  ;; start might be in the middle of a tab, which means we need to hide the
  ;; tab and add spaces
  (let ((end (+ start (length replacement)))
        beg-point end-point
        before-string after-string)
    (goto-char (point-at-eol))
    (if (< (current-column) start)
        (progn (setq before-string (make-string (- start (current-column)) ? ))
               (setq beg-point (point)))
      (goto-char (point-at-bol)) ;; Emacs bug, move-to-column is wrong otherwise
      (move-to-column start)
      (setq beg-point (point))
      (when (> (current-column) start)
        (goto-char (1- (point)))
        (setq beg-point (point))
        (setq before-string (make-string (- start (current-column)) ? ))))
    (move-to-column end)
    (setq end-point (point))
    (let ((end-offset (- (current-column) end)))
      (when (> end-offset 0) (setq after-string (make-string end-offset ?b))))
    (when no-insert
      ;; prevent inheriting of faces
      (setq before-string (when before-string (propertize before-string 'face 'default)))
      (setq after-string (when after-string (propertize after-string 'face 'default))))
    (let ((string (concat before-string replacement after-string)))
      (if no-insert
          string
        (push (dropdown-list-put-overlay beg-point end-point 'invisible t
                                         'after-string string)
              dropdown-list-overlays)))))

(defun dropdown-list-start-column (display-width)
  (let ((column (mod (current-column) (window-width)))
        (width (window-width)))
    (cond ((<= (+ column display-width) width) column)
          ((> column display-width) (- column display-width))
          ((>= width display-width) (- width display-width))
          (t nil))))

(defun dropdown-list-move-to-start-line (candidate-count)
  (decf candidate-count)
  (let ((above-line-count (save-excursion (- (vertical-motion (- candidate-count)))))
        (below-line-count (save-excursion (vertical-motion candidate-count))))
    (cond ((= below-line-count candidate-count)
           t)
          ((= above-line-count candidate-count)
           (vertical-motion (- candidate-count))
           t)
          ((>= (+ below-line-count above-line-count) candidate-count)
           (vertical-motion (- (- candidate-count below-line-count)))
           t)
          (t nil))))

(defun dropdown-list-at-point (candidates &optional selidx)
  (dropdown-list-hide)
  (let* ((lengths (mapcar #'length candidates))
         (max-length (apply #'max lengths))
         (start (dropdown-list-start-column (+ max-length 3)))
         (i -1)
         (candidates (mapcar* (lambda (candidate length)
                                (let ((diff (- max-length length)))
                                  (propertize
                                   (concat (if (> diff 0)
                                               (concat candidate (make-string diff ? ))
                                             (substring candidate 0 max-length))
                                           (format "%3d" (+ 2 i)))
                                   'face (if (eql (incf i) selidx)
                                             'dropdown-list-selection-face
                                           'dropdown-list-face))))
                              candidates
                              lengths)))
    (save-excursion
      (and start
           (dropdown-list-move-to-start-line (length candidates))
           (loop initially (vertical-motion 0)
                 for candidate in candidates
                 do (dropdown-list-line (+ (current-column) start) candidate)
                 while (/= (vertical-motion 1) 0)
                 finally return t)))))

(defun dropdown-list (candidates)
  (let ((selection)
        (temp-buffer))
    (save-window-excursion
      (unwind-protect
          (let ((candidate-count (length candidates))
                done key (selidx 0))
            (while (not done)
              (unless (dropdown-list-at-point candidates selidx)
                (switch-to-buffer (setq temp-buffer (get-buffer-create "*selection*"))
                                  'norecord)
                (delete-other-windows)
                (delete-region (point-min) (point-max))
                (insert (make-string (length candidates) ?\n))
                (goto-char (point-min))
                (dropdown-list-at-point candidates selidx))
              (setq key (read-key-sequence ""))
              (cond ((and (stringp key)
                          (>= (aref key 0) ?1)
                          (<= (aref key 0) (+ ?0 (min 9 candidate-count))))
                     (setq selection (- (aref key 0) ?1)
                           done      t))
                    ((member key `(,(char-to-string ?\C-p) [up] "p"))
                     (setq selidx (mod (+ candidate-count (1- (or selidx 0)))
                                       candidate-count)))
                    ((member key `(,(char-to-string ?\C-n) [down] "n"))
                     (setq selidx (mod (1+ (or selidx -1)) candidate-count)))
                    ((member key `(,(char-to-string ?\f))))
                    ((member key `(,(char-to-string ?\r) [return]))
                     (setq selection selidx
                           done      t))
                    (t (setq done t)))))
        (dropdown-list-hide)
        (and temp-buffer (kill-buffer temp-buffer)))
      ;;     (when selection
      ;;       (message "your selection => %d: %s" selection (nth selection candidates))
      ;;       (sit-for 1))
      selection)))

(defun define-key* (keymap key command)
  "Add COMMAND to the multiple-command binding of KEY in KEYMAP.
Use multiple times to bind different COMMANDs to the same KEY."
  (define-key keymap key (combine-command command (lookup-key keymap key))))

(defun combine-command (command defs)
  "$$$$$ FIXME - no doc string"
  (cond ((null defs) command)
        ((and (listp defs)
              (eq 'lambda (car defs))
              (= (length defs) 4)
              (listp (fourth defs))
              (eq 'command-selector (car (fourth defs))))
         (unless (member `',command (cdr (fourth defs)))
           (setcdr (fourth defs) (nconc (cdr (fourth defs)) `(',command))))
         defs)
        (t
         `(lambda () (interactive) (command-selector ',defs ',command)))))

(defvar command-selector-last-command nil "$$$$$ FIXME - no doc string")

(defun command-selector (&rest candidates)
  "$$$$$ FIXME - no doc string"
  (if (and (eq last-command this-command) command-selector-last-command)
      (call-interactively command-selector-last-command)
    (let* ((candidate-strings
            (mapcar (lambda (candidate)
                      (format "%s" (if (symbolp candidate)
                                       candidate
                                     (let ((s (format "%s" candidate)))
                                       (if (>= (length s) 7)
                                           (concat (substring s 0 7) "...")
                                         s)))))
                    candidates))
           (selection (dropdown-list candidate-strings)))
      (when selection
        (let ((cmd (nth selection candidates)))
          (call-interactively cmd)
          (setq command-selector-last-command cmd))))))

;;;;;;;;;;;;;;;;;;;;

(provide 'dropdown-list)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; dropdown-list.el ends here;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;      Auto-generated code         ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun yas/initialize-bundle ()
  "Initialize YASnippet and load snippets in the bundle.";;; snippets for text-mode
(yas/define-snippets 'text-mode
		     '(("email" "`(replace-regexp-in-string \"@\" \"@NOSPAM.\" user-mail-address)`" "(user's email)" nil nil nil nil nil)
		       ("time" "`(current-time-string)`" "(current time)" nil nil nil nil nil))
		     'nil)


;;; snippets for cc-mode
(yas/define-snippets 'cc-mode
		     '(("do" "do\n{\n    $0\n} while (${1:condition});" "do { ... } while (...)" nil nil nil nil nil)
		       ("for" "for (${1:int i = 0}; ${2:i < N}; ${3:++i})\n{\n    $0\n}" "for (...; ...; ...) { ... }" nil nil nil nil nil)
		       ("if" "if (${1:condition})\n{\n    $0\n}" "if (...) { ... }" nil nil nil nil nil)
		       ("inc" "#include \"$1\"\n" "#include \"...\"" nil nil nil nil nil)
		       ("inc" "#include <$1>\n" "#include <...>" nil nil nil nil nil)
		       ("main" "int main(int argc, char *argv[])\n{\n    $0\n    return 0;\n}\n" "int main(argc, argv) { ... }" nil nil nil nil nil)
		       ("once" "#ifndef ${1:_`(upcase (file-name-nondirectory (file-name-sans-extension (buffer-file-name))))`_H_}\n#define $1\n\n$0\n\n#endif /* $1 */" "#ifndef XXX; #define XXX; #endif" nil nil nil nil nil)
		       ("struct" "struct ${1:name}\n{\n    $0\n};" "struct ... { ... }" nil nil nil nil nil))
		     '(text-mode))


;;; snippets for c++-mode
(yas/define-snippets 'c++-mode
		     '(("beginend" "${1:v}.begin(), $1.end" "v.begin(), v.end()" nil nil nil nil nil)
		       ("class" "class ${1:Name}\n{\npublic:\n    ${1:$(yas/substr text \"[^: ]*\")}($2);\n    virtual ~${1:$(yas/substr text \"[^: ]*\")}();\n};" "class ... { ... }" nil nil nil nil nil)
		       ("ns" "namespace " "namespace ..." nil nil nil nil nil)
		       ("template" "template <typename ${T}>" "template <typename ...>" nil nil nil nil nil)
		       ("using" "using namespace ${std};\n$0" "using namespace ... " nil nil nil nil nil))
		     '(cc-mode))


;;; snippets for c-mode
(yas/define-snippets 'c-mode
		     '(("fopen" "FILE *${fp} = fopen(${\"file\"}, \"${r}\");\n" "FILE *fp = fopen(..., ...);" nil nil nil nil nil)
		       ("printf" "printf (\"${1:%s}\\\\n\"${1:$(if (string-match \"%\" text) \",\" \"\\);\")\n}$2${1:$(if (string-match \"%\" text) \"\\);\" \"\")}" "printf " nil nil nil nil nil))
		     '(cc-mode))


;;; snippets for csharp-mode
(yas/define-snippets 'csharp-mode
		     '(("attrib" "/// <summary>\n/// $3\n/// </summary>\nprivate $1 $2;\n" "private attribute ....;" nil nil nil nil nil)
		       ("attrib" "/// <summary>\n/// $3\n/// </summary>\nprivate $1 $2;\n\n/// <summary>\n/// $4\n/// </summary>\n/// <value>$5</value>\npublic $1 $2\n{\n    get {\n        return this.$2;\n    }\n    set {\n        this.$2 = value;\n    }\n}\n" "private attribute ....; public property ... ... { ... }" nil nil nil nil nil)
		       ("attrib" "/// <summary>\n/// $3\n/// </summary>\nprivate $1 ${2:$(if (> (length text) 0) (format \"_%s%s\" (downcase (substring text 0 1)) (substring text 1 (length text))) \"\")};\n\n/// <summary>\n/// ${3:Description}\n/// </summary>\n/// <value><c>$1</c></value>\npublic ${1:Type} ${2:Name}\n{\n    get {\n        return this.${2:$(if (> (length text) 0) (format \"_%s%s\" (downcase (substring text 0 1)) (substring text 1 (length text))) \"\")};\n    }\n    set {\n        this.${2:$(if (> (length text) 0) (format \"_%s%s\" (downcase (substring text 0 1)) (substring text 1 (length text))) \"\")} = value;\n    }\n}\n" "private _attribute ....; public Property ... ... { ... }" nil nil nil nil nil)
		       ("class" "${5:public} class ${1:Name}\n{\n    #region Ctor & Destructor\n    /// <summary>\n    /// ${3:Standard Constructor}\n    /// </summary>\n    public $1($2)\n    {\n    }\n\n    /// <summary>\n    /// ${4:Default Destructor}\n    /// </summary>    \n    public ~$1()\n    {\n    }\n    #endregion\n}\n" "class ... { ... }" nil nil nil nil nil)
		       ("comment" "/// <summary>\n/// $1\n/// </summary>\n" "/// <summary> ... </summary>" nil nil nil nil nil)
		       ("comment" "/// <param name=\"$1\">$2</param>\n" "/// <param name=\"...\"> ... </param>" nil nil nil nil nil)
		       ("comment" "/// <returns>$1</returns>\n" "/// <param name=\"...\"> ... </param>" nil nil nil nil nil)
		       ("comment" "/// <exception cref=\"$1\">$2</exception>\n" "/// <exception cref=\"...\"> ... </exception>" nil nil nil nil nil)
		       ("method" "/// <summary>\n/// ${5:Description}\n/// </summary>${2:$(if (string= (upcase text) \"VOID\") \"\" (format \"%s%s%s\" \"\\n/// <returns><c>\" text \"</c></returns>\"))}\n${1:public} ${2:void} ${3:MethodName}($4)\n{\n$0\n}\n" "public void Method { ... }" nil nil nil nil nil)
		       ("namespace" "namespace $1\n{\n$0\n}\n" "namespace .. { ... }" nil nil nil nil nil)
		       ("prop" "/// <summary>\n/// $5\n/// </summary>\n/// <value>$6</value>\n$1 $2 $3\n{\n    get {\n        return this.$4;\n    }\n    set {\n        this.$4 = value;\n    }\n}\n" "property ... ... { ... }" nil nil nil nil nil)
		       ("region" "#region $1\n$0\n#endregion\n" "#region ... #endregion" nil nil nil nil nil)
		       ("using" "using $1;\n" "using ...;" nil nil nil nil nil)
		       ("using" "using System;\n" "using System;" nil nil nil nil nil)
		       ("using" "using System.$1;\n" "using System....;" nil nil nil nil nil))
		     '(cc-mode))


;;; snippets for objc-mode
(yas/define-snippets 'objc-mode
		     '(("prop" "- (${1:id})${2:foo}\n{\n    return $2;\n}\n\n- (void)set${2:$(capitalize text)}:($1)aValue\n{\n    [$2 autorelease];\n    $2 = [aValue retain];\n}\n$0" "foo { ... } ; setFoo { ... }" nil nil nil nil nil))
		     '(cc-mode))


;;; snippets for css-mode
(yas/define-snippets 'css-mode
		     '(("bg" "background-color: #${1:DDD};" "background-color: ..." nil nil nil nil nil)
		       ("bg" "background-image: url($1);" "background-image: ..." nil nil nil nil nil)
		       ("bor" "border: ${1:1px} ${2:solid} #${3:999};" "border size style color" nil nil nil nil nil)
		       ("cl" "clear: $1;\n" "clear: ..." nil nil nil nil nil)
		       ("disp" "display: block;\n" "display: block" nil nil nil nil nil)
		       ("disp" "display: inline;\n" "display: inline" nil nil nil nil nil)
		       ("disp" "display: none;\n" "display: none" nil nil nil nil nil)
		       ("ff" "font-family: $1;\n" "font-family: ..." nil nil nil nil nil)
		       ("fs" "font-size: ${12px};\n" "font-size: ..." nil nil nil nil nil)
		       ("mar" "margin-bottom: $1;\n" "margin-bottom: ..." nil nil nil nil nil)
		       ("mar" "margin-left: $1;\n" "margin-left: ..." nil nil nil nil nil)
		       ("mar" "margin: $1;\n" "margin: ..." nil nil nil nil nil)
		       ("mar" "margin: ${top} ${right} ${bottom} ${left};\n" "margin top right bottom left" nil nil nil nil nil)
		       ("mar" "margin-right: $1;\n" "margin-right: ..." nil nil nil nil nil)
		       ("mar" "margin-top: $1;\n" "margin-top: ..." nil nil nil nil nil)
		       ("pad" "padding-bottom: $1;\n" "padding-bottom: ..." nil nil nil nil nil)
		       ("pad" "padding-left: $1;\n" "padding-left: ..." nil nil nil nil nil)
		       ("pad" "padding: $1;\n" "padding: ..." nil nil nil nil nil)
		       ("pad" "padding: ${top} ${right} ${bottom} ${left};\n" "padding: top right bottom left" nil nil nil nil nil)
		       ("pad" "padding-right: $1;\n" "padding-right: ..." nil nil nil nil nil)
		       ("pad" "padding-top: $1;\n" "padding-top: ..." nil nil nil nil nil))
		     '(text-mode))


;;; snippets for emacs-lisp-mode
(yas/define-snippets 'emacs-lisp-mode
		     '(("defun" "(defun $1 ()\n  \"thisandthat.\"\n  (interactive)\n  (let (var1)\n    (setq var1 some)\n    $0\n  )\n)" "function template" nil nil nil nil nil)
		       ("dired" ";; idiom for processing a list of files in dired's marked files\n \n;; suppose myProcessFile is your function that takes a file path\n;; and do some processing on the file\n\n(defun dired-myProcessFile ()\n  \"apply myProcessFile function to marked files in dired.\"\n  (interactive)\n  (require 'dired)\n  (mapc 'myProcessFile (dired-get-marked-files))\n)\n\n;; to use it, type M-x dired-myProcessFile\n" "process marked files in dired" nil nil nil nil nil)
		       ("file" "(defun doThisFile (fpath)\n  \"Process the file at path FPATH ...\"\n  (let ()\n    ;; create temp buffer without undo record or font lock. (more efficient)\n    ;; first space in temp buff name is necessary\n    (set-buffer (get-buffer-create \" myTemp\"))\n    (insert-file-contents fpath nil nil nil t)\n\n    ;; process it ...\n    ;; (goto-char 0) ; move to begining of file's content (in case it was open)\n    ;; ... do something here\n    ;; (write-file fpath) ;; write back to the file\n\n    (kill-buffer \" myTemp\")))\n" "a function that process a file" nil nil nil nil nil)
		       ("file" "(defun read-lines (filePath)\n  \"Return a list of lines in FILEPATH.\"\n  (with-temp-buffer\n    (insert-file-contents filePath)\n    (split-string\n     (buffer-string) \"\\n\" t)) )\n\n;; process all lines\n(mapc \n (lambda (aLine) \n   (message aLine) ; do your stuff here\n   )\n (read-lines \"inputFilePath\")\n)" "read lines of a file" nil nil nil nil nil)
		       ("find-replace" "(defun replace-html-chars-region (start end)\n  \"Replace “<” to “&lt;” and other chars in HTML.\nThis works on the current region.\"\n  (interactive \"r\")\n  (save-restriction \n    (narrow-to-region start end)\n    (goto-char (point-min))\n    (while (search-forward \"&\" nil t) (replace-match \"&amp;\" nil t))\n    (goto-char (point-min))\n    (while (search-forward \"<\" nil t) (replace-match \"&lt;\" nil t))\n    (goto-char (point-min))\n    (while (search-forward \">\" nil t) (replace-match \"&gt;\" nil t))\n    )\n  )\n" "find and replace on region" nil nil nil nil nil)
		       ("grabstring" "(setq $0 (buffer-substring-no-properties myStartPos myEndPos))\n" "grab buffer substring" nil nil nil nil nil)
		       ("grabthing" "(setq $0 (thing-at-point 'symbol))\n" "grab word under cursor" nil nil nil nil nil)
		       ("traverse_dir" ";; apply a function to all files in a dir\n(require 'find-lisp)\n(mapc 'my-process-file (find-lisp-find-files \"~/myweb/\" \"\\\\.html$\"))\n" "traversing a directory" nil nil nil nil nil)
		       ("word-or-region" ";; example of a command that works on current word or text selection\n(defun down-case-word-or-region ()\n  \"Lower case the current word or text selection.\"\n(interactive)\n(let (pos1 pos2 meat)\n  (if (and transient-mark-mode mark-active)\n      (setq pos1 (region-beginning)\n            pos2 (region-end))\n    (setq pos1 (car (bounds-of-thing-at-point 'symbol))\n          pos2 (cdr (bounds-of-thing-at-point 'symbol))))\n\n  ; now, pos1 and pos2 are the starting and ending positions\n  ; of the current word, or current text selection if exists\n\n  ;; put your code here.\n  $0\n  ;; Some example of things you might want to do\n  (downcase-region pos1 pos2) ; example of a func that takes region as args\n  (setq meat (buffer-substring-no-properties pos1 pos2)) ; grab the text.\n  (delete-region pos1 pos2) ; get rid of it\n  (insert \"newText\") ; insert your new text\n\n  )\n)\n" "Command that works on region or word" nil nil nil nil nil))
		     '(text-mode))


;;; snippets for erlang-mode
(yas/define-snippets 'erlang-mode
		     '(("after" "after\n    $1 -> $0\n" "after ... ->" nil nil nil nil nil)
		       ("begin" "begin\n    $0\nend\n" "begin ... end" nil nil nil nil nil)
		       ("beh" "-behaviour(${1:gen_server}).\n$0\n" "-behaviour(...)." nil nil nil nil nil)
		       ("case" "case $1 of\n    $0\nend\n" "case ... of ... end" nil nil nil nil nil)
		       ("compile" "-compile([${1:export_all}]).\n$0\n" "-compile(...)." nil nil nil nil nil)
		       ("def" "-define($1,$2).\n$0\n" "-define(...,...)." nil nil nil nil nil)
		       ("exp" "-export([${1:start/0}]).\n$0\n" "-export([])." nil nil nil nil nil)
		       ("fun" "fun ($1) -> $0 end\n" "fun (...) -> ... end" nil nil nil nil nil)
		       ("if" "if\n    $1 -> $2;\n    true -> $0\nend\n" "if ... -> ... ; true -> ... end" nil nil nil nil nil)
		       ("ifdef" "-ifdef($1).\n$0\n-endif.\n" "-ifdef(...). ... -endif." nil nil nil nil nil)
		       ("ifndef" "-ifndef($1).\n$0\n-endif.\n" "-ifndef(...). ... -endif." nil nil nil nil nil)
		       ("imp" "-import(${1:lists}, [${2:map/2, sum/1}]).\n$0\n" "-import([])." nil nil nil nil nil)
		       ("inc" "-include(\"$1\").\n$0\n" "-include(\"...\")." nil nil nil nil nil)
		       ("inc" "-include_lib(\"$1\").\n$0\n" "-include_lib(\"...\")." nil nil nil nil nil)
		       ("loop" "${1:loop}($2) ->\n    receive\n	${3:_} ->\n	    $1($2)\n    end.\n$0\n" "loop(...) -> receive _ -> loop(...) end." nil nil nil nil nil)
		       ("mod" "-module(${1:`(file-name-nondirectory\n              (file-name-sans-extension (or (buffer-file-name) (buffer-name))))`}).\n$0\n" "-module()." nil nil nil nil nil)
		       ("rcv" "receive\n    $1 -> $0\nend\n" "receive ... -> ... end" nil nil nil nil nil)
		       ("rcv" "receive\nafter\n    $1 -> $0\nend\n" "receive after ... -> ... end" nil nil nil nil nil)
		       ("rec" "-record($1,{$2}).\n$0\n" "-record(...,{...})." nil nil nil nil nil)
		       ("try" "try $1 of\n    $0\ncatch\nafter\nend\n" "try ... of ... catch after end" nil nil nil nil nil)
		       ("undef" "-undef($1).\n$0\n" "-undef(...)." nil nil nil nil nil))
		     '(text-mode))


;;; snippets for f90-mode
(yas/define-snippets 'f90-mode
		     '(("au" "automatic $0 \n" "automatic" nil nil nil nil nil)
		       ("bd" "block data $0\n" "block data" nil nil nil nil nil)
		       ("c" "continue $0\n" "continue" nil nil nil nil nil)
		       ("ch" "character $0\n" "character" nil nil nil nil nil)
		       ("cx" "complex $0\n" "complex" nil nil nil nil nil)
		       ("dc" "double complex $0\n" "double complex" nil nil nil nil nil)
		       ("do" "do while (${1:condition})\n   $0\nend do\n" "do while (...) end do" nil nil nil nil nil)
		       ("dp" "double precision $0\n" "double precision" nil nil nil nil nil)
		       ("eq" "equivalence $0\n" "equivalence" nil nil nil nil nil)
		       ("ib" "implicit byte $0\n" "implicit byte" nil nil nil nil nil)
		       ("ic" "implicit complex $0\n" "implicit complex" nil nil nil nil nil)
		       ("ich" "implicit character $0\n" "implicit character" nil nil nil nil nil)
		       ("if" "if ( ${1:condition} ) then\n   $0\nend if\n" "if then end if" nil nil nil nil nil)
		       ("ii" "implicit integer $0\n" "implicit integer " nil nil nil nil nil)
		       ("il" "implicit logical $0\n" "implicit logical" nil nil nil nil nil)
		       ("in" "implicit none\n" "implicit none" nil nil nil nil nil)
		       ("inc" "include $0\n" "include" nil nil nil nil nil)
		       ("intr" "intrinsic $0\n" "intrinsic" nil nil nil nil nil)
		       ("ir" "implicit real $0\n" "implicit real" nil nil nil nil nil)
		       ("l" "logical $0\n" "logical" nil nil nil nil nil)
		       ("pa" "parameter $0\n" "parameter" nil nil nil nil nil)
		       ("pr" "program ${1:name}\n  $0\nend program ${1:name}\n" "program ... end program ..." nil nil nil nil nil)
		       ("re" "read (${1:*},${2:*}) $0\n" "read (*,*)" nil nil nil nil nil)
		       ("st" "structure $0\n" "structure" nil nil nil nil nil)
		       ("su" "subroutine $0\n" "subroutine" nil nil nil nil nil)
		       ("wr" "write (${1:*},${2:*}) $0\n" "write (*,*)" nil nil nil nil nil))
		     '(text-mode))


;;; snippets for html-mode
(yas/define-snippets 'html-mode
		     '(("body" "<body$1>\n  $0\n</body>" "<body>...</body>" nil nil nil nil nil)
		       ("br" "<br />" "<br />" nil nil nil nil nil)
		       ("code" "<code>\n  $0\n</code>" "<code>...</code>" nil nil nil nil nil)
		       ("code" "<code class=\"$1\">\n  $0\n</code>" "<code class=\"...\">...</code>" nil nil nil nil nil)
		       ("dd" "<dd>$1</dd>" "<dd> ... </dd>" nil "list" nil nil nil)
		       ("div" "<div${1: id=\"${2:some_id}\"}${3: class=\"${4:some_class}\"}>$0</div> " "<div...>...</div>" nil nil nil nil nil)
		       ("div" "<div class=\"$1\">\n  $0\n</div>" "<div class=\"...\">...</div>" nil nil nil nil nil)
		       ("div" "<div id=\"$1\">\n  $0\n</div>" "<div id=\"...\">...</div>" nil nil nil nil nil)
		       ("div" "<div id=\"$1\" class=\"$2\">\n  $0\n</div>" "<div id=\"...\" class=\"...\">...</div>" nil nil nil nil nil)
		       ("dl" "<dl>\n    $0\n</dl>\n" "<dl> ... </dl>" nil "list" nil nil nil)
		       ("dl" "<dl id=\"$1\">\n    $0\n</dl>" "<dl> ... </dl>" nil "list" nil nil nil)
		       ("doctype" "<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.01//EN\" \"http://www.w3.org/TR/html4/strict.dtd\">" "Doctype HTML 4.01 Strict" nil "meta" nil nil nil)
		       ("doctype" "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Frameset//EN\" \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-frameset.dtd\">" "DocType XHTML 1.0 frameset" nil "meta" nil nil nil)
		       ("doctype" "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.1//EN\" \"http://www.w3.org/TR/xhtml11/DTD/xhtml11.dtd\">" "DocType XHTML 1.1" nil "meta" nil nil nil)
		       ("doctype" "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Strict//EN\" \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd\">" "DocType XHTML 1.0 Strict" nil "meta" nil nil nil)
		       ("doctype" "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Transitional//EN\" \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd\">" "DocType XHTML 1.0 Transitional" nil "meta" nil nil nil)
		       ("dov" "a mirror up here $3\n\n\n<dov ${1:id=\"${2:some_id and here comes another nested field: ${3:nested_shit}}\"}>\n    $0\n</dov>\n<dov $1>\n    actually some other shit and $3\n</dov>\n" "<dov...>...</dov>" nil nil nil nil nil)
		       ("dt" "<dt>$1</dt>" "<dt> ... </dt>" nil "list" nil nil nil)
		       ("form" "<form method=\"$1\" id=\"$2\" action=\"$3\">\n  $0\n</form>" "<form method=\"...\" id=\"...\" action=\"...\"></form>" nil nil nil nil nil)
		       ("h1" "<h1>$1</h1>" "<h1>...</h1>" nil "header" nil nil nil)
		       ("h2" "<h2>$1</h2>" "<h2>...</h2>" nil "header" nil nil nil)
		       ("h3" "<h3>$1</h3>" "<h3>...</h3>" nil "header" nil nil nil)
		       ("h4" "<h4>$1</h4>" "<h4>...</h4>" nil "header" nil nil nil)
		       ("h5" "<h5>$1</h5>" "<h5>...</h5>" nil "header" nil nil nil)
		       ("h6" "<h6>$1</h6>" "<h6>...</h6>" nil "header" nil nil nil)
		       ("head" "<head>\n  $0\n</head>" "<head>...</head>" nil nil nil nil nil)
		       ("hr" "<hr />\n" "<hr />" nil nil nil nil nil)
		       ("href" "<a href=\"$1\">$2</a>" "<a href=\"...\">...</a>" nil nil nil nil nil)
		       ("html" "<html>\n  $0\n</html>\n" "<html>...</html>" nil nil nil nil nil)
		       ("html" "<html xmlns=\"http://www.w3.org/1999/xhtml\" xml:lang=\"${1:en}\" lang=\"${2:en}\">\n  $0\n</html>\n" "<html xmlns=\"...\">...</html>" nil nil nil nil nil)
		       ("img" "<img src=\"$1\" class=\"$2\" alt=\"$3\" />" "<img src=\"...\" class=\"...\" alt=\"...\" />" nil nil nil nil nil)
		       ("input" "<input type=\"$1\" name=\"$2\" value=\"$3\" />" "<input ... />" nil nil nil nil nil)
		       ("li" "<li>$1</li>" "<li>...</li>" nil "list" nil nil nil)
		       ("li" "<li class=\"$1\">$2</li>" "<li class=\"...\">...</li>" nil "list" nil nil nil)
		       ("link" "<link rel=\"${1:stylesheet}\" href=\"${2:url}\" type=\"${3:text/css}\" media=\"${4:screen}\" />" "<link stylesheet=\"...\" />" nil nil nil nil nil)
		       ("link" "<!--[if IE]>\n<link rel=\"${1:stylesheet}\" href=\"${2:url}\" type=\"${3:text/css}\" media=\"${4:screen}\" />\n<![endif]-->" "<!--[if IE]><link stylesheet=\"...\" /><![endif]-->" nil nil nil nil nil)
		       ("mailto" "<a href=\"mailto:$1@$2\">$0</a>" "<a href=\"mailto:...@...\">...</a>" nil nil nil nil nil)
		       ("meta" "<meta name=\"${1:generator}\" content=\"${2:content}\" />" "<meta name=\"...\" content=\"...\" />" nil "meta" nil nil nil)
		       ("meta" "<meta name=\"${1:Content-Type}\" content=\"${2:text/html; charset=UTF-8}\" />" "<meta http-equiv=\"...\" content=\"...\" />" nil "meta" nil nil nil)
		       ("ol" "<ol>\n  $0\n</ol>" "<ol>...</ol>" nil "list" nil nil nil)
		       ("ol" "<ol class=\"$1\">\n  $0\n</ol>" "<ol class=\"...\">...</ol>" nil "list" nil nil nil)
		       ("ol" "<ol id=\"$1\">\n  $0\n</ol>" "<ol id=\"...\">...</ol>" nil "list" nil nil nil)
		       ("p" "<p>$1</p>" "<p>...</p>" nil nil nil nil nil)
		       ("pre" "<pre>\n  $0\n</pre>" "<pre>...</pre>" nil nil nil nil nil)
		       ("quote" "<blockquote>\n  $1\n</blockquote>" "<blockquote>...</blockquote>" nil nil nil nil nil)
		       ("script" "<script type=\"text/javascript\">\n  $0\n</script>" "<script type=\"text/javascript\">...</script> " nil nil nil nil nil)
		       ("script" "<script type=\"text/javascript\" src=\"$1\"></script>" "<script type=\"text/javascript\" src=\"...\"></script> " nil nil nil nil nil)
		       ("span" "<span>$1</span>" "<span>...</span>" nil nil nil nil nil)
		       ("span" "<span class=\"$1\">$2</span>" "<span class=\"...\">...</span>" nil nil nil nil nil)
		       ("span" "<span id=\"$1\">$2</span>" "<span id=\"...\">...</span>" nil nil nil nil nil)
		       ("style" "<style type=\"text/css\" media=\"${1:screen}\">\n  $0\n</style>" "<style type=\"text/css\" media=\"...\">...</style>" nil nil nil nil nil)
		       ("table" "<table width=\"$1\" cellspacing=\"$2\" cellpadding=\"$3\" border=\"$4\">\n  $0\n</table>" "<table ...>...</table>" nil "table" nil nil nil)
		       ("td" "<td$1>$2</td>" "<td>...</td>" nil "table" nil nil nil)
		       ("textarea" "<textarea name=\"$1\" id=\"$2\" rows=\"$3\" cols=\"$4\" tabindex=\"$5\"></textarea>" "<textarea ...></textarea>" nil nil nil nil nil)
		       ("th" "<th$1>$2</th>" "<th>...</th>" nil "table" nil nil nil)
		       ("title" "<title>$1</title>" "<title>...</title>" nil nil nil nil nil)
		       ("tr" "<tr>\n  $0\n</tr>" "<tr>...</tr>" nil "table" nil nil nil)
		       ("ul" "<ul>\n  $0\n</ul>" "<ul>...</ul>" nil "list" nil nil nil)
		       ("ul" "<ul class=\"$1\">\n  $0\n</ul>" "<ul class=\"...\">...</ul>" nil "list" nil nil nil)
		       ("ul" "<ul id=\"$1\">\n  $0\n</ul>" "<ul id=\"...\">...</ul>" nil "list" nil nil nil))
		     '(text-mode))


;;; snippets for latex-mode
(yas/define-snippets 'latex-mode
		     '(("begin" "\n\\begin{${1:environment}}\n$0\n\\end{$1}\n" "\\begin{environment} ... \\end{environment}" nil nil nil nil nil))
		     '(text-mode))


;;; snippets for markdown-mode
(yas/define-snippets 'markdown-mode
		     '(("+" "+ ${1:Text}\n+$0\n" "Unordered List" nil nil nil nil nil)
		       ("-" "- ${1:Text}\n-$0\n" "Unordered List" nil nil nil nil nil)
		       ("_" "_${1:Text}_ $0\n" "Emphasis" nil nil nil nil nil)
		       ("__" "**${1:Text}** $0\n" "Strong" nil nil nil nil nil)
		       ("`" "\\`${1:Code}\\` $0\n" "Inline Code" nil nil nil nil nil)
		       ("h1" "# ${1:Header 1} #\n\n$0\n" "Header 1 (#)" nil nil nil nil nil)
		       ("h1" "${1:Header 1}\n${1:$(make-string (string-width text) ?\\=)}\n\n$0\n" "Header 1 (=)" nil nil nil nil nil)
		       ("h2" "## ${1:Header 1} ##\n\n$0\n" "Header 2 (##)" nil nil nil nil nil)
		       ("h2" "${1:Header 2}\n${1:$(make-string (string-width text) ?\\-)}\n\n$0\n" "Header 2 (-)" nil nil nil nil nil)
		       ("h3" "### ${1:Header 3} ###\n\n$0\n" "Header 3" nil nil nil nil nil)
		       ("h4" "#### ${1:Header 4} ####\n\n$0\n" "Header 4" nil nil nil nil nil)
		       ("h5" "##### ${1:Header 5} #####\n\n$0\n" "Header 5" nil nil nil nil nil)
		       ("h6" "###### ${1:Header 6} ######\n\n$0\n" "Header 6" nil nil nil nil nil)
		       ("hr" "\n----------\n\n$0\n" "Horizontal Rule (-)" nil nil nil nil nil)
		       ("hr" "\n*******\n\n$0\n" "Horizontal Rule (*)" nil nil nil nil nil)
		       ("img" "![${1:Alt Text}](${2:URL} $3) $0\n" "Image" nil nil nil nil nil)
		       ("link" "[${1:Link Text}](${2:URL} $3) $0\n" "Link" nil nil nil nil nil)
		       ("ol" "${1:1}. ${2:Text}\n${1:$(number-to-string (1+ (string-to-number text)))}. $0\n" "Ordered List" nil nil nil nil nil)
		       ("rimg" "![${1:Alt Text}][$2] $0\n" "Referenced Image" nil nil nil nil nil)
		       ("rlb" "[${1:Reference}]: ${2:URL} $3\n$0\n" "Reference Label" nil nil nil nil nil)
		       ("rlink" "[${1:Link Text}][$2] $0\n" "Reference Link" nil nil nil nil nil))
		     '(text-mode))


;;; snippets for nxml-mode
(yas/define-snippets 'nxml-mode
		     '(("body" "<body$1>\n  $0\n</body>" "<body>...</body>" nil nil nil nil nil)
		       ("br" "<br />" "<br />" nil nil nil nil nil)
		       ("code" "<code>\n  $0\n</code>" "<code>...</code>" nil nil nil nil nil)
		       ("div" "<div$1>$0</div>" "<div...>...</div>" nil nil nil nil nil)
		       ("doctype" "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.1//EN\" \"http://www.w3.org/TR/xhtml11/DTD/xhtml11.dtd\">" "DocType XHTML 1.1" nil "meta" nil nil nil)
		       ("doctype" "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Strict//EN\" \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd\">" "DocType XHTML 1.0 Strict" nil "meta" nil nil nil)
		       ("doctype" "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Transitional//EN\" \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd\">" "DocType XHTML 1.0 Transitional" nil "meta" nil nil nil)
		       ("form" "<form method=\"$1\" action=\"$2\">\n  $0\n</form>" "<form method=\"...\" action=\"...\"></form>" nil nil nil nil nil)
		       ("h1" "<h1>$1</h1>" "<h1>...</h1>" nil "header" nil nil nil)
		       ("h2" "<h2>$1</h2>" "<h2>...</h2>" nil "header" nil nil nil)
		       ("h3" "<h3>$1</h3>" "<h3>...</h3>" nil "header" nil nil nil)
		       ("h4" "<h4>$1</h4>" "<h4>...</h4>" nil "header" nil nil nil)
		       ("h5" "<h5>$1</h5>" "<h5>...</h5>" nil "header" nil nil nil)
		       ("h6" "<h6>$1</h6>" "<h6>...</h6>" nil "header" nil nil nil)
		       ("head" "<head>\n  $0\n</head>" "<head>...</head>" nil nil nil nil nil)
		       ("hr" "<hr />\n" "<hr />" nil nil nil nil nil)
		       ("href" "<a href=\"$1\">$2</a>" "<a href=\"...\">...</a>" nil nil nil nil nil)
		       ("html" "<html xmlns=\"http://www.w3.org/1999/xhtml\" xml:lang=\"${1:en}\" lang=\"${2:en}\">\n  $0\n</html>\n" "<html xmlns=\"...\">...</html>" nil nil nil nil nil)
		       ("img" "<img src=\"$1\" alt=\"$2\" />" "<img src=\"...\" alt=\"...\" />" nil nil nil nil nil)
		       ("input" "<input type=\"$1\" name=\"$2\" value=\"$3\" />" "<input ... />" nil nil nil nil nil)
		       ("li" "<li>$1</li>" "<li>...</li>" nil nil nil nil nil)
		       ("link" "<link rel=\"${1:stylesheet}\" href=\"${2:url}\" type=\"${3:text/css}\" media=\"${4:screen}\" />" "<link stylesheet=\"...\" />" nil nil nil nil nil)
		       ("meta" "<meta name=\"${1:generator}\" content=\"${2:content}\" />" "<meta name=\"...\" content=\"...\" />" nil "meta" nil nil nil)
		       ("name" "<a name=\"$1\"></a>" "<a name=\"...\"></a>" nil nil nil nil nil)
		       ("ol" "<ol>\n  $0\n</ol>" "<ol>...</ol>" nil nil nil nil nil)
		       ("p" "<p>$1</p>" "<p>...</p>" nil nil nil nil nil)
		       ("pre" "<pre>\n  $0\n</pre>" "<pre>...</pre>" nil nil nil nil nil)
		       ("quote" "<blockquote>\n  $1\n</blockquote>" "<blockquote>...</blockquote>" nil nil nil nil nil)
		       ("span" "<span>$1</span>" "<span>...</span>" nil nil nil nil nil)
		       ("style" "<style type=\"text/css\" media=\"${1:screen}\">\n  $0\n</style>" "<style type=\"text/css\" media=\"...\">...</style>" nil nil nil nil nil)
		       ("table" "<table>\n  $0\n</table>" "<table>...</table>" nil nil nil nil nil)
		       ("tag" "<${1:tag}>$2</$1>$0" "<tag>...</tag>" nil nil nil nil nil)
		       ("tag" "<${1:tag}>\n  $2\n</$1>$0" "<tag> \\n...\\n</tag>" nil nil nil nil nil)
		       ("td" "<td$1>$2</td>" "<td>...</td>" nil nil nil nil nil)
		       ("th" "<th$1>$2</th>" "<th>...</th>" nil nil nil nil nil)
		       ("title" "<title>$1</title>" "<title>...</title>" nil nil nil nil nil)
		       ("tr" "<tr>\n  $0\n</tr>" "<tr>...</tr>" nil nil nil nil nil)
		       ("ul" "<ul>\n  $0\n</ul>" "<ul>...</ul>" nil nil nil nil nil))
		     '(text-mode))


;;; snippets for perl-mode
(yas/define-snippets 'perl-mode
		     '(("eval" "eval {\n    ${1:# do something risky...}\n};\nif (\\$@) {\n    ${2:# handle failure...}\n}" "eval { ... } if ($@) { ... }" nil nil nil nil nil)
		       ("for" "for (my \\$${1:var} = 0; \\$$1 < ${2:expression}; \\$$1++) {\n    ${3:# body...}\n}" "for (...) { ... }" nil nil nil nil nil)
		       ("fore" "foreach my \\$${1:x} (@${2:array}) {\n    ${3:# body...}\n}" "foreach ... { ... }" nil nil nil nil nil)
		       ("if" "if ($1) {\n    $0\n}" "if (...) { ... }" nil nil nil nil nil)
		       ("ife" "if ($1) {\n    $2\n} else {\n    $3\n}" "if (...) { ... } else { ... }" nil nil nil nil nil)
		       ("ifee" "if ($1) {\n	${2:# body...}\n} elsif ($3) {\n	${4:# elsif...}\n} else {\n	${5:# else...}\n}" "if, elsif, else ..." nil nil nil nil nil)
		       ("sub" "sub ${1:function_name} {\n    $0\n}" "sub ... { ... }" nil nil nil nil nil)
		       ("unless" "unless ($1) {\n    $0\n}" "unless (...) { ... }" nil nil nil nil nil)
		       ("while" "while ($1) {\n    $0\n}" "while (...) { ... }" nil nil nil nil nil)
		       ("xfore" "${1:expression} foreach @${2:array};" "... foreach ..." nil nil nil nil nil)
		       ("xif" "${1:expression} if ${2:condition}" "... if ..." nil nil nil nil nil)
		       ("xunless" "${1:expression} unless ${2:condition}" "... unless ..." nil nil nil nil nil)
		       ("xwhile" "${1:expression} while ${2:condition};" "... while ..." nil nil nil nil nil))
		     '(text-mode))


;;; snippets for cperl-mode
(yas/define-snippets 'cperl-mode 'nil
		     '(perl-mode))


;;; snippets for python-mode
(yas/define-snippets 'python-mode
		     '(("__" "__${init}__" "__...__" nil nil nil nil nil)
		       ("class" "class ${1:ClassName}(${2:object}):\n    \"\"\"$3\n    \"\"\"\n\n    def __init__(self, $4):\n        \"\"\"$5\n        ${4:$\n        (let* ((indent\n                (concat \"\\n\" (make-string (current-column) 32)))\n               (args\n                (mapconcat\n                 '(lambda (x)\n                    (if (not (string= (nth 0 x) \"\"))\n                        (concat \"- \" (char-to-string 96) (nth 0 x)\n                                (char-to-string 96) \":\")))\n                 (mapcar\n                  '(lambda (x)\n                     (mapcar\n                      (lambda (x)\n                        (replace-regexp-in-string \"[[:blank:]]*$\" \"\"\n                         (replace-regexp-in-string \"^[[:blank:]]*\" \"\" x))) x))\n                  (mapcar '(lambda (x) (split-string x \"=\"))\n                          (split-string text \",\")))\n                 indent)))\n          (if (string= args \"\")\n              (make-string 3 34)\n            (mapconcat\n             'identity\n             (list \"\" \"Arguments:\" args (make-string 3 34))\n             indent)))\n        }\n        ${4:$\n        (mapconcat\n         '(lambda (x)\n            (if (not (string= (nth 0 x) \"\"))\n                (concat \"self._\" (nth 0 x) \" = \" (nth 0 x))))\n         (mapcar\n          '(lambda (x)\n             (mapcar\n              '(lambda (x)\n                 (replace-regexp-in-string \"[[:blank:]]*$\" \"\"\n                  (replace-regexp-in-string \"^[[:blank:]]*\" \"\" x)))\n              x))\n          (mapcar '(lambda (x) (split-string x \"=\"))\n                  (split-string text \",\")))\n         (concat \"\\n\" (make-string (current-column) 32)))\n        }\n        $0\n" "class" nil nil nil nil nil)
		       ("def" "def ${1:name}($2):\n    \"\"\"$3\n    ${2:$\n      (let* \n        ((indent\n            (concat \"\\n\" (make-string (current-column) 32)))\n           (args\n            (mapconcat\n             '(lambda (x)\n                (if (not (string= (nth 0 x) \"\"))\n                    (concat \"- \" (char-to-string 96) (nth 0 x)\n                            (char-to-string 96) \":\")))\n             (mapcar\n              '(lambda (x)\n                 (mapcar\n                  '(lambda (x)\n                     (replace-regexp-in-string \"[[:blank:]]*$\" \"\"\n                      (replace-regexp-in-string \"^[[:blank:]]*\" \"\" x)))\n                  x))\n              (mapcar '(lambda (x) (split-string x \"=\"))\n                      (split-string text \",\")))\n             indent)))\n      (if (string= args \"\")\n          (make-string 3 34)\n        (mapconcat\n         'identity\n         (list \"\" \"Arguments:\" args (make-string 3 34))\n         indent)))\n    }\n    $0\n" "def" nil nil nil nil nil)
		       ("defm" "def ${1:name}(self, $2):\n    \"\"\"$3\n    ${2:$\n    (let* ((indent\n            (concat \"\\n\" (make-string (current-column) 32)))\n           (args\n            (mapconcat\n             '(lambda (x)\n                (if (not (string= (nth 0 x) \"\"))\n                    (concat \"- \" (char-to-string 96) (nth 0 x)\n                            (char-to-string 96) \":\")))\n             (mapcar\n              '(lambda (x)\n                 (mapcar\n                  '(lambda (x)\n                     (replace-regexp-in-string \"[[:blank:]]*$\" \"\"\n                      (replace-regexp-in-string \"^[[:blank:]]*\" \"\" x)))\n                  x))\n              (mapcar '(lambda (x) (split-string x \"=\"))\n                      (split-string text \",\")))\n             indent)))\n      (if (string= args \"\")\n          (make-string 3 34)\n        (mapconcat\n         'identity\n         (list \"\" \"Arguments:\" args (make-string 3 34))\n         indent)))\n    }\n    $0\n" "defm" nil nil nil nil nil)
		       ("for" "for ${var} in ${collection}:\n    $0" "for ... in ... : ..." nil nil nil nil nil)
		       ("ifmain" "if __name__ == '__main__':\n    $0" "if __name__ == '__main__': ..." nil nil nil nil nil)
		       ("prop" "def ${1:foo}():\n   doc = \"\"\"${2:Doc string}\"\"\"\n   def fget(self):\n       return self._$1\n   def fset(self, value):\n       self._$1 = value\n   def fdel(self):\n       del self._$1\n   return locals()\n$1 = property(**$1())\n\n$0\n" "prop" nil nil nil nil nil)
		       ("propg" "def _get_${1:foo}(self):\n    return self._$1\n\n$1 = property(_get_$1)\n\n$0\n" "_get_foo ... foo=property(...)" nil nil nil nil nil)
		       ("propsg" "def _set_${1:foo}(self, value):\n    self._$1 = value\n\ndef _get_$1(self):\n    return self._$1\n\n$1 = property(_get_$1, _set_$1)\n\n$0\n" "_get_foo ... _set_foo ... foo=property(...)" nil nil nil nil nil)
		       ("while" "while ${condition}:\n    $0" "while ... : ..." nil nil nil nil nil))
		     '(text-mode))


;;; snippets for rst-mode
(yas/define-snippets 'rst-mode
		     '(("chap" "${1:Chapter}\n${1:$(make-string (string-width text) ?\\=)}\n\n$0" "Chapter title" nil nil nil nil nil)
		       ("sec" "${1:Section}\n${1:$(make-string (string-width text) ?\\-)}\n\n$0" "Section title" nil nil nil nil nil)
		       ("tit" "${1:$(make-string (string-width text) ?\\=)}\n${1:Title}\n${1:$(make-string (string-width text) ?\\=)}\n\n$0" "Document title" nil nil nil nil nil))
		     '(text-mode))


;;; snippets for ruby-mode
(yas/define-snippets 'ruby-mode
		     '(("#" "# => " "# =>" nil "general" nil nil nil)
		       ("=b" "=begin rdoc\n  $0\n=end" "=begin rdoc ... =end" nil "general" nil nil nil)
		       ("Comp" "include Comparable\n\ndef <=> other\n  $0\nend" "include Comparable; def <=> ... end" nil "definitions" nil nil nil)
		       ("all" "all? { |${e}| $0 }" "all? { |...| ... }" nil "collections" nil nil nil)
		       ("am" "alias_method :${new_name}, :${old_name}" "alias_method new, old" nil "definitions" nil nil nil)
		       ("any" "any? { |${e}| $0 }" "any? { |...| ... }" nil "collections" nil nil nil)
		       ("app" "if __FILE__ == $PROGRAM_NAME\n  $0\nend" "if __FILE__ == $PROGRAM_NAME ... end" nil "general" nil nil nil)
		       ("bm" "Benchmark.bmbm(${1:10}) do |x|\n  $0\nend" "Benchmark.bmbm(...) do ... end" nil "general" nil nil nil)
		       ("case" "case ${1:object}\nwhen ${2:condition}\n  $0\nend" "case ... end" nil "general" nil nil nil)
		       ("cla" "class << ${self}\n  $0\nend" "class << self ... end" nil "definitions" nil nil nil)
		       ("classify" "classify { |${e}| $0 }" "classify { |...| ... }" nil "collections" nil nil nil)
		       ("cls" "class ${1:`(let ((fn (capitalize (file-name-nondirectory\n                                 (file-name-sans-extension\n				 (or (buffer-file-name)\n				     (buffer-name (current-buffer))))))))\n           (cond\n             ((string-match \"_\" fn) (replace-match \"\" nil nil fn))\n              (t fn)))`}\n  $0\nend\n" "class ... end" nil "definitions" nil nil nil)
		       ("collect" "collect { |${e}| $0 }" "collect { |...| ... }" nil "collections" nil nil nil)
		       ("dee" "Marshal.load(Marshal.dump($0))" "deep_copy(...)" nil "general" nil nil nil)
		       ("deli" "delete_if { |${e} $0 }" "delete_if { |...| ... }" nil "collections" nil nil nil)
		       ("det" "detect { |${e}| $0 }" "detect { |...| ... }" nil "collections" nil nil nil)
		       ("dow" "downto(${0}) { |${n}|\n  $0\n}" "downto(...) { |n| ... }" nil "control structure" nil nil nil)
		       ("ea" "each { |${e}| $0 }" "each { |...| ... }" nil "collections" nil nil nil)
		       ("eac" "each_cons(${1:2}) { |${group}| $0 }" "each_cons(...) { |...| ... }" nil "collections" nil nil nil)
		       ("eai" "each_index { |${i}| $0 }" "each_index { |i| ... }" nil "collections" nil nil nil)
		       ("eav" "each_value { |${val}| $0 }" "each_value { |val| ... }" nil "collections" nil nil nil)
		       ("eawi" "each_with_index { |${e}, ${i}| $0 }" "each_with_index { |e, i| ... }" nil "collections" nil nil nil)
		       ("forin" "for ${1:element} in ${2:collection}\n  $0\nend" "for ... in ...; ... end" nil "control structure" nil nil nil)
		       ("if" "if ${1:condition}\n  $0\nend" "if ... end" nil "control structure" nil nil nil)
		       ("ife" "if ${1:condition}\n  $2\nelse\n  $3\nend" "if ... else ... end" nil "control structure" nil nil nil)
		       ("inject" "inject(${1:0}) { |${2:injection}, ${3:element}| $0 }" "inject(...) { |...| ... }" nil "collections" nil nil nil)
		       ("mm" "def method_missing(method, *args)\n  $0\nend" "def method_missing ... end" nil "definitions" nil nil nil)
		       ("r" "attr_reader :" "attr_reader ..." nil "definitions" nil nil nil)
		       ("rb" "#!/usr/bin/ruby -wKU\n" "/usr/bin/ruby -wKU" nil "general" nil nil nil)
		       ("reject" "reject { |${1:element}| $0 }" "reject { |...| ... }" nil "collections" nil nil nil)
		       ("req" "require \"$0\"" "require \"...\"" nil "general" nil nil nil)
		       ("rreq" "require File.join(File.dirname(__FILE__), $0)" "require File.join(File.dirname(__FILE__), ...)" nil "general" nil nil nil)
		       ("rw" "attr_accessor :" "attr_accessor ..." nil "definitions" nil nil nil)
		       ("select" "select { |${1:element}| $0 }" "select { |...| ... }" nil "collections" nil nil nil)
		       ("tim" "times { |${n}| $0 }" "times { |n| ... }" nil "control structure" nil nil nil)
		       ("until" "until ${condition}\n  $0\nend" "until ... end" nil "control structure" nil nil nil)
		       ("upt" "upto(${n}) { |${i}|\n  $0\n}" "upto(...) { |n| ... }" nil "control structure" nil nil nil)
		       ("w" "attr_writer :" "attr_writer ..." nil "definitions" nil nil nil)
		       ("when" "when ${condition}\n  $0\nend" "when ... end" nil "control structure" nil nil nil)
		       ("while" "while ${condition}\n  $0\nend" "while ... end" nil "control structure" nil nil nil)
		       ("y" ":yields: $0" ":yields: arguments (rdoc)" nil "general" nil nil nil)
		       ("zip" "zip(${enums}) { |${row}| $0 }" "zip(...) { |...| ... }" nil "collections" nil nil nil))
		     '(text-mode))


;;; snippets for scala-mode
(yas/define-snippets 'scala-mode
		     '(("act" "def act = {\n  loop {\n    react {\n      $0\n    }\n  }\n}" "def act = { ..}" nil nil nil nil nil)
		       ("act" "def act(${1:arg}: ${2:type}) = {\n  loop {\n    react {\n      $0\n    }\n  }\n}" "def act(arg: T) = { ..}" nil nil nil nil nil)
		       ("actor" "val a = actor {\n  loop {\n    react {\n      $0\n    }\n  }\n}" "val a = actor { ..}" nil nil nil nil nil)
		       ("ano" "($1) => ${2:body} $0" "(args) => ..." nil nil nil nil nil)
		       ("app" "object ${1:name} extends Application {\n  $0\n}" "object name extends Application" nil nil nil nil nil)
		       ("arr" "Array[${1:value}](${2:args}) $0" "Array[T](..)" nil nil nil nil nil)
		       ("arr" "val ${1:arr} = Array[${2:value}](${3:args}) $0" "val a = Array[T](..)" nil nil nil nil nil)
		       ("asof" "asInstanceOf[${1:type}] $0" "asInstanceOf[T] " nil nil nil nil nil)
		       ("ass" "assert(${1:x} === ${2:y}) $0" "assert(x === y)" nil nil nil nil nil)
		       ("ass" "assert(true) $0" "assert(true)" nil nil nil nil nil)
		       ("at" "@author ${1:name} $0" "@author name" nil nil nil nil nil)
		       ("at" "@param ${1:name} ${2:description} $0" "@param name description" nil nil nil nil nil)
		       ("at" "@return ${1:description} $0" "@return description" nil nil nil nil nil)
		       ("at" "@version ${1:0.1} $0" "@version number" nil nil nil nil nil)
		       ("bang" "${1:actor} ! ${2:message} $0" "actor ! message" nil nil nil nil nil)
		       ("case" "case ${1:pattern} => $0" "case pattern => " nil nil nil nil nil)
		       ("case" "case _ => $0" "case _ => " nil nil nil nil nil)
		       ("cast" "asInstanceOf[${1:type}] $0" "asInstanceOf[T] " nil nil nil nil nil)
		       ("cc" "case class ${1:name}(${2:arg}: ${3:type}) $0" "case class T(arg: A)" nil nil nil nil nil)
		       ("cl" "class ${1:name} {\n  $0\n}" "class T { .. }" nil nil nil nil nil)
		       ("cl" "abstract class ${1:name} {\n  $0\n}" "abstract class T { .. }" nil nil nil nil nil)
		       ("cl" "abstract class ${1:name}(${2:args}) {\n  $0\n}" "abstract class T(args) { .. }" nil nil nil nil nil)
		       ("cl" "class ${1:name}(${2:args}) {\n  $0\n}" "class T(args) { .. }" nil nil nil nil nil)
		       ("clof" "classOf[${1:type}] $0" "classOf[T] " nil nil nil nil nil)
		       ("co" "case object ${1:name} $0" "case object T" nil nil nil nil nil)
		       ("cons" "${1:element1} :: ${2:element2} $0" "element1 :: element2" nil nil nil nil nil)
		       ("cons" "${1:element1} :: Nil $0\n" "element1 :: Nil" nil nil nil nil nil)
		       ("def" "def ${1:name}(${2:args}) = $0" "def f(arg: T) = ..." nil nil nil nil nil)
		       ("def" "def ${1:name}(${2:args}) = {\n  $0\n}" "def f(arg: T) = {...}" nil nil nil nil nil)
		       ("def" "def ${1:name}(${2:args}): ${3:Unit} = $0" "def f(arg: T): R = ..." nil nil nil nil nil)
		       ("def" "def ${1:name}(${2:args}): ${3:Unit} = {\n  $0\n}" "def f(arg: T): R = {...}" nil nil nil nil nil)
		       ("def" "def ${1:name} = {\n  $0\n}" "def f = {...}" nil nil nil nil nil)
		       ("def" "def ${1:name}: ${2:Unit} = $0" "def f: R = ..." nil nil nil nil nil)
		       ("def" "def ${1:name}: ${3:Unit} = {\n  $0\n}" "def f: R = {...}" nil nil nil nil nil)
		       ("def" "def ${1:name} = $0" "def f = ..." nil nil nil nil nil)
		       ("doc" "/** \n * `(scala-mode-find-clstrtobj-name-doc)`\n * ${1:description}\n * $0  \n */" "/** cls/trt/obj name */" nil nil nil nil nil)
		       ("doc" "/** \n * `(scala-mode-def-and-args-doc)`\n */ " "/** method name */" nil nil nil nil nil)
		       ("doc" "/**\n * `(scala-mode-file-doc)`\n * $0\n * @author ${1:name}\n * @version ${2:0.1} \n */" "/** file name */" nil nil nil nil nil)
		       ("doc" "/*                     __                                               *\\\n**     ________ ___   / /  ___     Scala $3                               **\n**    / __/ __// _ | / /  / _ |    (c) 2005-`(format-time-string \"%Y\")` , LAMP/EPFL             **\n**  __\\ \\/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **\n** /____/\\___/_/ |_/____/_/ | |                                         **\n**                          |/                                          **\n\\*                                                                      */\n/** \n * $0\n * @author ${1:name} \n * @version ${2:0.1}\n * $Id$\n */" "/** scala file */" nil nil nil nil nil)
		       ("doc" "/*                     __                                               *\\\n**     ________ ___   / /  ___     Scala API                            **\n**    / __/ __// _ | / /  / _ |    (c) 2005-`(format-time-string \"%Y\")`, LAMP/EPFL             **\n**  __\\ \\/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **\n** /____/\\___/_/ |_/____/_/ | |                                         **\n**                          |/                                          **\n\\*                                                                      */\n/** \n * $0\n * @author ${1:name} \n * @version ${2:0.1}\n * $Id$\n */" "/** scala api file */" nil nil nil nil nil)
		       ("doc" "/**\n * ${1:description}\n * $0\n */" "/** ... */" nil nil nil nil nil)
		       ("expect" "expect(${1:reply}) {\n  $0\n}" "expect(value) { ..}" nil nil nil nil nil)
		       ("ext" "extends $0" "extends T" nil nil nil nil nil)
		       ("for" "${1:x} <- ${2:xs}" "x <- xs" nil nil nil nil nil)
		       ("for" "for (${1:x} <- ${2:xs} if ${3:guard}) {\n  $0\n}" "for (x <- xs if guard) { ... }" nil nil nil nil nil)
		       ("for" "for (${1:x} <- ${2:xs}) {\n  $0\n}" "for (x <- xs) { ... }" nil nil nil nil nil)
		       ("for" "for {\n  ${1:x} <- ${2:xs}\n  ${3:x} <- ${4:xs}\n} {\n  yield $0\n}" "for {x <- xs \\ y <- ys} { yield }" nil nil nil nil nil)
		       ("foreach" "foreach(${1:x} => ${2:body}) $0" "foreach(x => ..)" nil nil nil nil nil)
		       ("hmap" "new HashMap[${1:key}, ${2:value}] $0" "new HashMap[K, V]" nil nil nil nil nil)
		       ("hmap" "val ${1:m} = new HashMap[${2:key}, ${3:value}] $0" "val m = new HashMap[K, V]" nil nil nil nil nil)
		       ("hset" "new HashSet[${1:key}] $0\n" "new HashSet[K]" nil nil nil nil nil)
		       ("hset" "val ${1:m} = new HashSet[${2:key}] $0" "val m = new HashSet[K]" nil nil nil nil nil)
		       ("if" "if (${1:condition}) {\n  $0\n}" "if (cond) { .. }" nil nil nil nil nil)
		       ("if" "if (${1:condition}) {\n  $2\n} else {\n  $0\n}" "if (cond) { .. } else { .. }" nil nil nil nil nil)
		       ("imp" "import $0" "import .." nil nil nil nil nil)
		       ("intercept" "intercept(classOf[${1:Exception]}) {\n  $0\n}" "intercept(classOf[T]) { ..}" nil nil nil nil nil)
		       ("isof" "isInstanceOf[${1:type}] $0" "isInstanceOf[T] " nil nil nil nil nil)
		       ("ls" "List(${1:args}, ${2:args}) $0" "List(..)" nil nil nil nil nil)
		       ("ls" "val ${1:l} = List(${2:args}, ${3:args}) $0" "val l = List(..)" nil nil nil nil nil)
		       ("main" "def main(args: Array[String]) = {\n  $0\n}" "def main(args: Array[String]) = { ... }" nil nil nil nil nil)
		       ("map" "map(${1:x} => ${2:body}) $0" "map(x => ..)" nil nil nil nil nil)
		       ("map" "Map(${1:key} -> ${2:value}) $0" "Map(key -> value)" nil nil nil nil nil)
		       ("match" "${1:cc} match {\n  case ${2:pattern} => $0\n}" "cc match { .. }" nil nil nil nil nil)
		       ("match" "${1:option} match {\n  case Full(res) => $0\n\n  case Empty => \n\n  case Failure(msg, _, _) => \n\n}" "can match { case Full(res) => .. }" nil nil nil nil nil)
		       ("match" "${1:option} match {\n  case None => $0\n  case Some(res) => \n\n}" "option match { case None => .. }" nil nil nil nil nil)
		       ("mix" "trait ${1:name} {\n  $0\n}" "trait T { .. }" nil nil nil nil nil)
		       ("ob" "object ${1:name} extends ${2:type} $0" "object name extends T" nil nil nil nil nil)
		       ("pac" "package $0" "package .." nil nil nil nil nil)
		       ("pr" "println(${1:obj}) $0" "println(..)" nil nil nil nil nil)
		       ("pr" "print(${1:obj}) $0" "print(..)" nil nil nil nil nil)
		       ("pr" "println(\"${1:msg}\") $0" "println(\"..\")" nil nil nil nil nil)
		       ("pr" "println(\"${1:obj}: \" + ${1:obj}) $0" "println(\"obj: \" + obj)" nil nil nil nil nil)
		       ("pri" "private $0" "private" nil nil nil nil nil)
		       ("pri" "private[${1:this}] $0" "private[this]" nil nil nil nil nil)
		       ("pro" "protected $0" "protected" nil nil nil nil nil)
		       ("pro" "protected[${1:this}] $0" "protected[this]" nil nil nil nil nil)
		       ("suite" "import org.scalatest._\n\nclass ${1:name} extends Suite {\n  $0\n}" "class T extends Suite { .. }" nil nil nil nil nil)
		       ("test" "//@Test\ndef test${1:name} = {\n  $0\n}" "@Test def testX = ..." nil nil nil nil nil)
		       ("throw" "throw new ${1:Exception}(${2:msg}) $0" "throw new Exception" nil nil nil nil nil)
		       ("tr" "trait ${1:name} {\n  $0\n}" "trait T { .. }" nil nil nil nil nil)
		       ("tr" "trait ${1:name} extends ${2:class} {\n  $0\n}" "trait T extends C { .. }" nil nil nil nil nil)
		       ("tr" "trait ${1:name} extends ${2:class} with ${3:trait} {\n  $0\n}" "trait T1 extends C with T2 { .. }" nil nil nil nil nil)
		       ("tr" "trait ${1:name} with ${2:trait} {\n  $0\n}" "trait T1 with T2 { .. }" nil nil nil nil nil)
		       ("try" "try {\n  $0\n} catch {\n  case ${1:e}: ${2:Exception} => \n    ${1:println(\\\"ERROR: \\\" + e) // TODO: handle exception}\\n}\n}" "try { .. } catch { case e => ..}" nil nil nil nil nil)
		       ("try" "try {\n  $0\n} catch {\n  case ${1:e}: ${2:Exception} => \n    ${1:println(\\\"ERROR: \\\" + e) // TODO: handle exception}\\n}\n} finally {\n\n}" "try { .. } catch { case e => ..} finally { ..}" nil nil nil nil nil)
		       ("try" "try {\n\n} finally {\n  $0\n}" "try { .. } finally { .. }" nil nil nil nil nil)
		       ("tup" "${1:element1} -> ${2:element2} $0" "element1 -> element2" nil nil nil nil nil)
		       ("tup" "(${1:element1}, ${2:element2}) $0" "(element1, element2)" nil nil nil nil nil)
		       ("val" "val ${1:name} = ${2:obj} $0" "val name = .." nil nil nil nil nil)
		       ("val" "val ${1:name} = new ${2:obj} $0" "val name = new .." nil nil nil nil nil)
		       ("val" "val ${1:name}: ${2:T} = ${3:obj} $0\n" "val name: T = .." nil nil nil nil nil)
		       ("var" "var ${1:name} = ${2:obj} $0\n" "var name = .." nil nil nil nil nil)
		       ("var" "var ${1:name} = new ${2:obj} $0\n" "var name = new .." nil nil nil nil nil)
		       ("var" "var ${1:name}: ${2:T} = ${3:obj} $0\n" "var name: T = .." nil nil nil nil nil)
		       ("whi" "while (${1:condition}) {\n  $0\n}" "while(cond) { .. }" nil nil nil nil nil)
		       ("with" "with $0" "with T" nil nil nil nil nil))
		     '(text-mode))


;;; snippets for snippet-mode
(yas/define-snippets 'snippet-mode
		     '(("$f" "\\${${1:${2:n}:}$3${4:\\$(${5:lisp-fn})}\\}$0" "${ ...  } field" nil nil nil nil nil)
		       ("$m" "\\${${2:n}:${4:\\$(${5:reflection-fn})}\\}$0" "${n:$(...)} mirror" nil nil nil nil nil)
		       ("vars" "# name : $1${2:\n# key : ${3:expand-key}}${4:\n# group : ${5:group}} \n# contributor : $6\n# --\n$0" "Snippet header" nil nil nil nil nil))
		     '(text-mode))


;;; snippets for sql-mode
(yas/define-snippets 'sql-mode
		     '(("column" "	,	${1:Name}		${2:Type}			${3:NOT NULL}\n" ", ColumnName ColumnType NOT NULL..." nil nil nil nil nil)
		       ("constraint" "CONSTRAINT [${1:PK_Name}] PRIMARY KEY ${2:CLUSTERED} ([${3:ColumnName}]) \n" "CONSTRAINT [..] PRIMARY KEY ..." nil nil nil nil nil)
		       ("constraint" "CONSTRAINT [${1:FK_Name}] FOREIGN KEY ${2:CLUSTERED} ([${3:ColumnName}]) \n" "CONSTRAINT [..] FOREIGN KEY ..." nil nil nil nil nil)
		       ("create" "CREATE TABLE [${1:dbo}].[${2:TableName}] \n(\n		${3:Id}		${4:INT IDENTITY(1,1)}		${5:NOT NULL}\n$0\n	CONSTRAINT [${6:PK_}] PRIMARY KEY ${7:CLUSTERED} ([$3]) \n)\nGO\n" "create table ..." nil nil nil nil nil)
		       ("create" "CREATE PROCEDURE [${1:dbo}].[${2:Name}] \n(\n		$3		$4		= ${5:NULL}		${6:OUTPUT}\n)\nAS\nBEGIN\n$0\nEND\nGO\n" "create procedure ..." nil nil nil nil nil)
		       ("references" "REFERENCES ${1:TableName}([${2:ColumnName}])\n" "REFERENCES ..." nil nil nil nil nil))
		     '(text-mode))


(yas/global-mode 1)
)

(yas/initialize-bundle)
;;;###autoload(require 'yasnippet-bundle)
(set-default 'yas/dont-activate
	     #'(lambda nil
		 (and
		  (or yas/root-directory
		      (featurep 'yasnippet-bundle))
		  (null
		   (yas/get-snippet-tables)))))
(provide 'yasnippet-bundle)
;;; yasnippet-bundle.el ends here
